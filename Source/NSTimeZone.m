/** Time zone management. -*- Mode: ObjC -*-
   Copyright (C) 1997-20 11Free Software Foundation, Inc.

   Written by: Yoo C. Chung <wacko@laplace.snu.ac.kr>
   Date: June 1997

   Rewrite large chunks by: Richard Frith-Macdonald <rfm@gnu.org>
   Date: September 2002

     This file is part of the GNUstep Base Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110 USA.

   <title>NSTimeZone class reference</title>
   $Date$ $Revision$
 */

/* Use the system time zones if available. In other cases, use an
   implementation independent of the system, since POSIX functions for
   time zones are woefully inadequate for implementing NSTimeZone.
   Time zone names can be different from system to system, but usually
   the user has already set up his timezone independant of GNUstep, so we
   should respect that information.

   We do not use a dictionary for storing time zones, since such a
   dictionary would be VERY large (~500K).  And we would have to use a
   complicated object determining whether we're using daylight savings
   time and such for every entry in the dictionary.  (Though we will
   eventually have to change the implementation to prevent the year
   2038 problem.)

   The local time zone can be specified (with the ones listed first
   having precedence) by:

    1) the user defaults database
    2) the GNUSTEP_TZ environment variable
    3) the file LOCAL_TIME_FILE in _time_zone_path()

   Failing that, the time zone may be guessed from system dependent sources
   such as:

    the windows registry
    the /etc/timezone file
    the /etc/sysconfig/clock file
    TZDEFAULT defined in tzfile.h
    the TZ environment variable
    tzset() & tznam[]/daylight

    If all else faile, the fallback time zone (which is GMT/UTC)

   Any time zone must be a file name in ZONES_DIR.

   Files & File System Heirarchy info:
   ===================================

   Default place for the NSTimeZone directory is _time_zone_path():
     {$(GNUSTEP_SYSTEM_LIBRARY)/Libraries/gnustep-base/Versions/???/Resources/TIME_ZONE_DIR)

   LOCAL_TIME_FILE is a text file with the name of the time zone file.

   ZONES_DIR is a sub-directory under TIME_ZONE_DIR

   (dir) ../System/Library/Libraries/gnustep-base/Versions/???/Resources/..
   (dir)     NSTimeZone
   (file)      localtime {text; time zone eg Australia/Perth}
   (dir)       zones

   Note that full zone info is required, especially the various "GMT"
   files which are created especially for OPENSTEP compatibility.
   Zone info comes from the Olson time database.

   FIXME?: use leap seconds? */

#import "common.h"
#define	EXPOSE_NSTimeZone_IVARS	1
#import "GNUstepBase/GSLock.h"
#include <stdio.h>
#include <time.h>
#import "Foundation/NSArray.h"
#import "Foundation/NSCoder.h"
#import "Foundation/NSData.h"
#import "Foundation/NSDate.h"
#import "Foundation/NSDictionary.h"
#import "Foundation/NSException.h"
#import "Foundation/NSFileManager.h"
#import "Foundation/NSLock.h"
#import "Foundation/NSProcessInfo.h"
#import "Foundation/NSUserDefaults.h"
#import "Foundation/NSMapTable.h"
#import "Foundation/NSThread.h"
#import "Foundation/NSNotification.h"
#import "Foundation/NSPortCoder.h"
#import "Foundation/NSTimeZone.h"
#import "Foundation/NSByteOrder.h"
#import "Foundation/NSLocale.h"
#import "GNUstepBase/NSString+GNUstepBase.h"
#import "GSPrivate.h"
#import "GSPThread.h"

#ifdef HAVE_TZHEAD
#include <tzfile.h>
#else
#include "nstzfile.h"
#endif

#if defined(HAVE_UNICODE_UCAL_H)
#define id id_ucal
#include <unicode/ucal.h>
#undef id
#endif

/* Key for local time zone in user defaults. */
#define LOCALDBKEY @"Local Time Zone"

/* Directory that contains the time zone data.
   Expected in Resources directory for library bundle. */
#define TIME_ZONE_DIR @"NSTimeZones"

/* Name of time zone abbreviation (plist) dictionary.  */
#define ABBREV_DICT @"abbreviations"

/* Name of time zone abbreviation map.  It is a text file
   with each line comprised of the abbreviation, a whitespace, and the
   name.  Neither the abbreviation nor the name can contain
   whitespace, and each line must not be longer than 80 characters. */
#define ABBREV_MAP @"abbreviations"

/* File holding regions grouped by latitude.  It is a text file with
   each line comprised of the latitude region, whitespace, and the
   name.  Neither the abbreviation nor the name can contain
   whitespace, and each line must not be longer than 80 characters. */
#define REGIONS_FILE @"regions"

/* Name of the file that contains the name of the local time zone. */
#define LOCAL_TIME_FILE @"localtime"

/* Directory that contains the actual time zones. */
#define ZONES_DIR @"zones"

/* Many systems have this file */
#define SYSTEM_TIME_FILE @"/etc/localtime"

/* If TZDIR told us where the zoneinfo files are, don't append anything else */
#ifdef TZDIR
#define POSIX_TZONES     @""
#else
#define POSIX_TZONES     @"posix/"
#endif

#define BUFFER_SIZE 512
#define WEEK_MILLISECONDS (7.0*24.0*60.0*60.0*1000.0)

/* 
 * Here starts public-domain code stolen from NetBSD's libc
 * src/lib/libc/time/localtime.c
 */

/*	$NetBSD: localtime.c,v 1.123 2020/05/25 14:52:48 christos Exp $	*/

/* Convert timestamp from time_t to struct tm.  */

/*
** This file is in the public domain, so clarified as of
** 1996-06-05 by Arthur David Olson.
*/

/*
** Leap second handling from Bradley White.
** POSIX-style TZ environment variable handling from Guy Harris.
*/

#include "tzdb_private.h"

#include <fcntl.h>

#ifndef TZ_ABBR_MAX_LEN
#define TZ_ABBR_MAX_LEN	16
#endif /* !defined TZ_ABBR_MAX_LEN */

#ifndef TZ_ABBR_CHAR_SET
#define TZ_ABBR_CHAR_SET \
	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 :+-._"
#endif /* !defined TZ_ABBR_CHAR_SET */

#ifndef TZ_ABBR_ERR_CHAR
#define TZ_ABBR_ERR_CHAR	'_'
#endif /* !defined TZ_ABBR_ERR_CHAR */

/*
** SunOS 4.1.1 headers lack O_BINARY.
*/

#ifdef O_BINARY
#define OPEN_MODE	(O_RDONLY | O_BINARY | O_CLOEXEC)
#endif /* defined O_BINARY */
#ifndef O_BINARY
#define OPEN_MODE	(O_RDONLY | O_CLOEXEC)
#endif /* !defined O_BINARY */

/*
** The DST rules to use if TZ has no rules and we can't load TZDEFRULES.
** Default to US rules as of 2017-05-07.
** POSIX does not specify the default DST rules;
** for historical reasons, US rules are a common default.
*/
#ifndef TZDEFRULESTRING
#define TZDEFRULESTRING ",M3.2.0,M11.1.0"
#endif

struct _ttinfo {				/* time type information */
	int_fast32_t	tt_utoff;	/* UT offset in seconds */
	bool		tt_isdst;	/* used to set tm_isdst */
	int		tt_desigidx;	/* abbreviation list index */
	bool		tt_ttisstd;	/* transition is std time */
	bool		tt_ttisut;	/* transition is UT */
};

struct lsinfo {				/* leap second information */
	time_t		ls_trans;	/* transition time */
	int_fast64_t	ls_corr;	/* correction to apply */
};

#define SMALLEST(a, b)	(((a) < (b)) ? (a) : (b))
#define BIGGEST(a, b)	(((a) > (b)) ? (a) : (b))

#ifdef TZNAME_MAX
#define MY_TZNAME_MAX	TZNAME_MAX
#endif /* defined TZNAME_MAX */
#ifndef TZNAME_MAX
#define MY_TZNAME_MAX	255
#endif /* !defined TZNAME_MAX */

#define state __state
struct state {
	int		leapcnt;
	int		timecnt;
	int		typecnt;
	int		charcnt;
	bool		goback;
	bool		goahead;
	time_t		ats[TZ_MAX_TIMES];
	unsigned char	types[TZ_MAX_TIMES];
	struct _ttinfo	ttis[TZ_MAX_TYPES];
	char		chars[/*CONSTCOND*/BIGGEST(BIGGEST(TZ_MAX_CHARS + 1,
				sizeof "GMT"), (2 * (MY_TZNAME_MAX + 1)))];
	struct lsinfo	lsis[TZ_MAX_LEAPS];

	/* The time type to use for early times or if no transitions.
	   It is always zero for recent tzdb releases.
	   It might be nonzero for data from tzdb 2018e or earlier.  */
	int defaulttype;
	NSData		*data;
};

enum r_type {
  JULIAN_DAY,		/* Jn = Julian day */
  DAY_OF_YEAR,		/* n = day of year */
  MONTH_NTH_DAY_OF_WEEK	/* Mm.n.d = month, week, day of week */
};

struct rule {
	enum r_type	r_type;		/* type of rule */
	int		r_day;		/* day number of rule */
	int		r_week;		/* week number of rule */
	int		r_mon;		/* month number of rule */
	int_fast32_t	r_time;		/* transition time of rule */
};

static bool increment_overflow(int *, int);
static bool increment_overflow_time(time_t *, int_fast32_t);
static int_fast64_t leapcorr(struct state const *, time_t);
static struct tm *timesub(time_t const *, int_fast32_t, struct state const *,
			  struct tm *);
static bool typesequiv(struct state const *, int, int);
static bool tzparse(char const *, struct state *, bool);

/* Initialize *S to a value based on UTOFF, ISDST, and DESIGIDX.  */
static void
init_ttinfo(struct _ttinfo *s, int_fast32_t utoff, bool isdst, int desigidx)
{
	s->tt_utoff = utoff;
	s->tt_isdst = isdst;
	s->tt_desigidx = desigidx;
	s->tt_ttisstd = false;
	s->tt_ttisut = false;
}

static int_fast32_t
detzcode(const char *const codep)
{
	int_fast32_t result;
	int	i;
	int_fast32_t one = 1;
	int_fast32_t halfmaxval = one << (32 - 2);
	int_fast32_t maxval = halfmaxval - 1 + halfmaxval;
	int_fast32_t minval = -1 - maxval;

	result = codep[0] & 0x7f;
	for (i = 1; i < 4; ++i)
		result = (result << 8) | (codep[i] & 0xff);

	if (codep[0] & 0x80) {
	  /* Do two's-complement negation even on non-two's-complement machines.
	     If the result would be minval - 1, return minval.  */
	    result -= !TWOS_COMPLEMENT(int_fast32_t) && result != 0;
	    result += minval;
	}
 	return result;
}

static int_fast64_t
detzcode64(const char *const codep)
{
	int_fast64_t result;
	int	i;
	int_fast64_t one = 1;
	int_fast64_t halfmaxval = one << (64 - 2);
	int_fast64_t maxval = halfmaxval - 1 + halfmaxval;
	int_fast64_t minval = -TWOS_COMPLEMENT(int_fast64_t) - maxval;

	result = codep[0] & 0x7f;
	for (i = 1; i < 8; ++i)
		result = (result << 8) | (codep[i] & 0xff);

	if (codep[0] & 0x80) {
	  /* Do two's-complement negation even on non-two's-complement machines.
	     If the result would be minval - 1, return minval.  */
	  result -= !TWOS_COMPLEMENT(int_fast64_t) && result != 0;
	  result += minval;
	}
 	return result;
}



static void
scrub_abbrs(struct state *sp)
{
	int i;

	/*
	** First, replace bogus characters.
	*/
	for (i = 0; i < sp->charcnt; ++i)
		if (strchr(TZ_ABBR_CHAR_SET, sp->chars[i]) == NULL)
			sp->chars[i] = TZ_ABBR_ERR_CHAR;
	/*
	** Second, truncate long abbreviations.
	*/
	for (i = 0; i < sp->typecnt; ++i) {
		const struct _ttinfo * const	ttisp = &sp->ttis[i];
		char *cp = &sp->chars[ttisp->tt_desigidx];

		if (strlen(cp) > TZ_ABBR_MAX_LEN &&
			strcmp(cp, GRANDPARENTED) != 0)
				*(cp + TZ_ABBR_MAX_LEN) = '\0';
	}
}

static bool
differ_by_repeat(const time_t t1, const time_t t0)
{
	if (TYPE_BIT(time_t) - TYPE_SIGNED(time_t) < SECSPERREPEAT_BITS)
		return 0;
	return (int_fast64_t)t1 - (int_fast64_t)t0 == SECSPERREPEAT;
}

union input_buffer {
	/* The first part of the buffer, interpreted as a header.  */
	struct tzhead tzhead;

	/* The entire buffer.  */
	char buf[2 * sizeof(struct tzhead) + 2 * sizeof (struct state)
	  + 4 * TZ_MAX_TIMES];
};

/* TZDIR with a trailing '/' rather than a trailing '\0'.  */
static char const tzdirslash[sizeof TZDIR] = TZDIR "/";

/* Local storage needed for 'tzloadbody'.  */
union local_storage {
	/* The results of analyzing the file's contents after it is opened.  */
	struct file_analysis {
		/* The input buffer.  */
		union input_buffer u;

		/* A temporary state used for parsing a TZ string in the file.  */
		struct state st;
	} u;

	/* The file name to be opened.  */
	char fullname[/*CONSTCOND*/BIGGEST(sizeof (struct file_analysis),
	    sizeof tzdirslash + 1024)];
};

/* Load tz data from the file named NAME into *SP.  Read extended
   format if DOEXTEND.  Use *LSP for temporary storage.  Return 0 on
   success, an errno value on failure.  */
static int
tzloadbody(char const *name, struct state *sp, bool doextend,
  union local_storage *lsp)
{
	int			i;
	int			fid;
	int			stored;
	ssize_t			nread;
	bool			doaccess;
	union input_buffer	*up = &lsp->u.u;
	size_t			tzheadsize = sizeof(struct tzhead);

	sp->goback = sp->goahead = false;

	if (! name) {
		name = TZDEFAULT;
		if (! name)
			return EINVAL;
	}

	if (name[0] == ':')
		++name;
#ifdef SUPPRESS_TZDIR
	/* Do not prepend TZDIR.  This is intended for specialized
	   applications only, due to its security implications.  */
	doaccess = true;
#else
	doaccess = name[0] == '/';
#endif
	if (!doaccess) {
		char const *dot;
		size_t namelen = strlen(name);
		if (sizeof lsp->fullname - sizeof tzdirslash <= namelen)
			return ENAMETOOLONG;

		/* Create a string "TZDIR/NAME".  Using sprintf here
		   would pull in stdio (and would fail if the
		   resulting string length exceeded INT_MAX!).  */
		memcpy(lsp->fullname, tzdirslash, sizeof tzdirslash);
		strcpy(lsp->fullname + sizeof tzdirslash, name);

		/* Set doaccess if NAME contains a ".." file name
		   component, as such a name could read a file outside
		   the TZDIR virtual subtree.  */
		for (dot = name; (dot = strchr(dot, '.')) != NULL; dot++)
		  if ((dot == name || dot[-1] == '/') && dot[1] == '.'
		      && (dot[2] == '/' || !dot[2])) {
		    doaccess = true;
		    break;
		  }

		name = lsp->fullname;
	}
	if (doaccess && access(name, R_OK) != 0)
		return errno;

	fid = open(name, OPEN_MODE);
	if (fid < 0)
		return errno;
	nread = read(fid, up->buf, sizeof up->buf);
	if (nread < (ssize_t)tzheadsize) {
		int err = nread < 0 ? errno : EINVAL;
		close(fid);
		return err;
	}
	if (close(fid) < 0)
		return errno;

	sp->data = [NSData dataWithBytes: up->buf length: nread];

	for (stored = 4; stored <= 8; stored *= 2) {
		int_fast32_t ttisstdcnt = detzcode(up->tzhead.tzh_ttisstdcnt);
		int_fast32_t ttisutcnt = detzcode(up->tzhead.tzh_ttisutcnt);
		int_fast64_t prevtr = 0;
		int_fast32_t prevcorr = 0;
		int_fast32_t leapcnt = detzcode(up->tzhead.tzh_leapcnt);
		int_fast32_t timecnt = detzcode(up->tzhead.tzh_timecnt);
		int_fast32_t typecnt = detzcode(up->tzhead.tzh_typecnt);
		int_fast32_t charcnt = detzcode(up->tzhead.tzh_charcnt);
		char const *p = up->buf + tzheadsize;
		/* Although tzfile(5) currently requires typecnt to be nonzero,
		   support future formats that may allow zero typecnt
		   in files that have a TZ string and no transitions.  */
		if (! (0 <= leapcnt && leapcnt < TZ_MAX_LEAPS
		       && 0 <= typecnt && typecnt < TZ_MAX_TYPES
		       && 0 <= timecnt && timecnt < TZ_MAX_TIMES
		       && 0 <= charcnt && charcnt < TZ_MAX_CHARS
		       && (ttisstdcnt == typecnt || ttisstdcnt == 0)
		       && (ttisutcnt == typecnt || ttisutcnt == 0)))
		  return EINVAL;
		if ((size_t)nread
		    < (tzheadsize		/* struct tzhead */
		       + timecnt * stored	/* ats */
		       + timecnt		/* types */
		       + typecnt * 6		/* _ttinfos */
		       + charcnt		/* chars */
		       + leapcnt * (stored + 4)	/* lsinfos */
		       + ttisstdcnt		/* ttisstds */
		       + ttisutcnt))		/* ttisuts */
		  return EINVAL;
		sp->leapcnt = leapcnt;
		sp->timecnt = timecnt;
		sp->typecnt = typecnt;
		sp->charcnt = charcnt;

		/* Read transitions, discarding those out of time_t range.
		   But pretend the last transition before TIME_T_MIN
		   occurred at TIME_T_MIN.  */
		timecnt = 0;
		for (i = 0; i < sp->timecnt; ++i) {
			int_fast64_t at
			  = stored == 4 ? detzcode(p) : detzcode64(p);
			sp->types[i] = at <= TIME_T_MAX;
			if (sp->types[i]) {
				time_t attime
				    = ((TYPE_SIGNED(time_t) ?
				    at < TIME_T_MIN : at < 0)
				    ? TIME_T_MIN : (time_t)at);
				if (timecnt && attime <= sp->ats[timecnt - 1]) {
					if (attime < sp->ats[timecnt - 1])
						return EINVAL;
					sp->types[i - 1] = 0;
					timecnt--;
				}
				sp->ats[timecnt++] = attime;
			}
			p += stored;
		}

		timecnt = 0;
		for (i = 0; i < sp->timecnt; ++i) {
			unsigned char typ = *p++;
			if (sp->typecnt <= typ)
			  return EINVAL;
			if (sp->types[i])
				sp->types[timecnt++] = typ;
		}
		sp->timecnt = timecnt;
		for (i = 0; i < sp->typecnt; ++i) {
			struct _ttinfo *	ttisp;
			unsigned char isdst, desigidx;

			ttisp = &sp->ttis[i];
			ttisp->tt_utoff = detzcode(p);
			p += 4;
			isdst = *p++;
			if (! (isdst < 2))
				return EINVAL;
			ttisp->tt_isdst = isdst;
			desigidx = *p++;
			if (! (desigidx < sp->charcnt))
				return EINVAL;
			ttisp->tt_desigidx = desigidx;
		}
		for (i = 0; i < sp->charcnt; ++i)
			sp->chars[i] = *p++;
		sp->chars[i] = '\0';	/* ensure '\0' at end */

		/* Read leap seconds, discarding those out of time_t range.  */
		leapcnt = 0;
		for (i = 0; i < sp->leapcnt; ++i) {
			int_fast64_t tr = stored == 4 ? detzcode(p) :
			    detzcode64(p);
			int_fast32_t corr = detzcode(p + stored);
			p += stored + 4;
			/* Leap seconds cannot occur before the Epoch.  */
			if (tr < 0)
				return EINVAL;
			if (tr <= TIME_T_MAX) {
		    /* Leap seconds cannot occur more than once per UTC month,
		       and UTC months are at least 28 days long (minus 1
		       second for a negative leap second).  Each leap second's
		       correction must differ from the previous one's by 1
		       second.  */
				if (tr - prevtr < 28 * SECSPERDAY - 1
				    || (corr != prevcorr - 1
				    && corr != prevcorr + 1))
					  return EINVAL;

				sp->lsis[leapcnt].ls_trans =
				    (time_t)(prevtr = tr);
				sp->lsis[leapcnt].ls_corr = prevcorr = corr;
				leapcnt++;
			}
		}
		sp->leapcnt = leapcnt;

		for (i = 0; i < sp->typecnt; ++i) {
			struct _ttinfo *	ttisp;

			ttisp = &sp->ttis[i];
			if (ttisstdcnt == 0)
				ttisp->tt_ttisstd = false;
			else {
				if (*p != true && *p != false)
				  return EINVAL;
				ttisp->tt_ttisstd = *p++;
			}
		}
		for (i = 0; i < sp->typecnt; ++i) {
			struct _ttinfo *	ttisp;

			ttisp = &sp->ttis[i];
			if (ttisutcnt == 0)
				ttisp->tt_ttisut = false;
			else {
				if (*p != true && *p != false)
						return EINVAL;
				ttisp->tt_ttisut = *p++;
			}
		}
		/*
		** If this is an old file, we're done.
		*/
		if (up->tzhead.tzh_version[0] == '\0')
			break;
		nread -= p - up->buf;
		memmove(up->buf, p, (size_t)nread);
	}
	if (doextend && nread > 2 &&
		up->buf[0] == '\n' && up->buf[nread - 1] == '\n' &&
		sp->typecnt + 2 <= TZ_MAX_TYPES) {
			struct state *ts = &lsp->u.st;

			up->buf[nread - 1] = '\0';
			if (tzparse(&up->buf[1], ts, false)) {

			  /* Attempt to reuse existing abbreviations.
			     Without this, America/Anchorage would be right on
			     the edge after 2037 when TZ_MAX_CHARS is 50, as
			     sp->charcnt equals 40 (for LMT AST AWT APT AHST
			     AHDT YST AKDT AKST) and ts->charcnt equals 10
			     (for AKST AKDT).  Reusing means sp->charcnt can
			     stay 40 in this example.  */
			  int gotabbr = 0;
			  int charcnt = sp->charcnt;
			  for (i = 0; i < ts->typecnt; i++) {
			    char *tsabbr = ts->chars + ts->ttis[i].tt_desigidx;
			    int j;
			    for (j = 0; j < charcnt; j++)
			      if (strcmp(sp->chars + j, tsabbr) == 0) {
				ts->ttis[i].tt_desigidx = j;
				gotabbr++;
				break;
			      }
			    if (! (j < charcnt)) {
			      size_t tsabbrlen = strlen(tsabbr);
			      if (j + tsabbrlen < TZ_MAX_CHARS) {
				strcpy(sp->chars + j, tsabbr);
				charcnt = (int_fast32_t)(j + tsabbrlen + 1);
				ts->ttis[i].tt_desigidx = j;
				gotabbr++;
			      }
			    }
			  }
			  if (gotabbr == ts->typecnt) {
			    sp->charcnt = charcnt;

			    /* Ignore any trailing, no-op transitions generated
			       by zic as they don't help here and can run afoul
			       of bugs in zic 2016j or earlier.  */
			    while (1 < sp->timecnt
				   && (sp->types[sp->timecnt - 1]
				       == sp->types[sp->timecnt - 2]))
			      sp->timecnt--;

			    for (i = 0; i < ts->timecnt; i++)
			      if (sp->timecnt == 0
				  || (sp->ats[sp->timecnt - 1]
				      < ts->ats[i] + leapcorr(sp, ts->ats[i])))
				break;
			    while (i < ts->timecnt
				   && sp->timecnt < TZ_MAX_TIMES) {
			      sp->ats[sp->timecnt] = (time_t)
				(ts->ats[i] + leapcorr(sp, ts->ats[i]));
			      sp->types[sp->timecnt] = (sp->typecnt
							+ ts->types[i]);
			      sp->timecnt++;
			      i++;
			    }
			    for (i = 0; i < ts->typecnt; i++)
			      sp->ttis[sp->typecnt++] = ts->ttis[i];
			  }
			}
	}
	if (sp->typecnt == 0)
	  return EINVAL;
	if (sp->timecnt > 1) {
		for (i = 1; i < sp->timecnt; ++i)
			if (typesequiv(sp, sp->types[i], sp->types[0]) &&
				differ_by_repeat(sp->ats[i], sp->ats[0])) {
					sp->goback = true;
					break;
				}
		for (i = sp->timecnt - 2; i >= 0; --i)
			if (typesequiv(sp, sp->types[sp->timecnt - 1],
				sp->types[i]) &&
				differ_by_repeat(sp->ats[sp->timecnt - 1],
				sp->ats[i])) {
					sp->goahead = true;
					break;
		}
	}

	/* Infer sp->defaulttype from the data.  Although this default
	   type is always zero for data from recent tzdb releases,
	   things are trickier for data from tzdb 2018e or earlier.

	   The first set of heuristics work around bugs in 32-bit data
	   generated by tzdb 2013c or earlier.  The workaround is for
	   zones like Australia/Macquarie where timestamps before the
	   first transition have a time type that is not the earliest
	   standard-time type.  See:
	   https://mm.icann.org/pipermail/tz/2013-May/019368.html */
	/*
	** If type 0 is unused in transitions,
	** it's the type to use for early times.
	*/
	for (i = 0; i < sp->timecnt; ++i)
		if (sp->types[i] == 0)
			break;
	i = i < sp->timecnt ? -1 : 0;
	/*
	** Absent the above,
	** if there are transition times
	** and the first transition is to a daylight time
	** find the standard type less than and closest to
	** the type of the first transition.
	*/
	if (i < 0 && sp->timecnt > 0 && sp->ttis[sp->types[0]].tt_isdst) {
		i = sp->types[0];
		while (--i >= 0)
			if (!sp->ttis[i].tt_isdst)
				break;
	}
	/* The next heuristics are for data generated by tzdb 2018e or
	   earlier, for zones like EST5EDT where the first transition
	   is to DST.  */
	/*
	** If no result yet, find the first standard type.
	** If there is none, punt to type zero.
	*/
	if (i < 0) {
		i = 0;
		while (sp->ttis[i].tt_isdst)
			if (++i >= sp->typecnt) {
				i = 0;
				break;
			}
	}
	/* A simple 'sp->defaulttype = 0;' would suffice here if we
	   didn't have to worry about 2018e-or-earlier data.  Even
	   simpler would be to remove the defaulttype member and just
	   use 0 in its place.  */
	sp->defaulttype = i;

	return 0;
}

/* Load tz data from the file named NAME into *SP.  Read extended
   format if DOEXTEND.  Return 0 on success, an errno value on failure.  */
static int
tzload(char const *name, struct state *sp, bool doextend)
{
	union local_storage *lsp = malloc(sizeof *lsp);
	if (!lsp)
		return errno;
	else {
		int err = tzloadbody(name, sp, doextend, lsp);
		free(lsp);
		return err;
	}
}

static bool
typesequiv(const struct state *sp, int a, int b)
{
	bool result;

	if (sp == NULL ||
		a < 0 || a >= sp->typecnt ||
		b < 0 || b >= sp->typecnt)
			result = false;
	else {
		const struct _ttinfo *	ap = &sp->ttis[a];
		const struct _ttinfo *	bp = &sp->ttis[b];
		result = (ap->tt_utoff == bp->tt_utoff
			  && ap->tt_isdst == bp->tt_isdst
			  && ap->tt_ttisstd == bp->tt_ttisstd
			  && ap->tt_ttisut == bp->tt_ttisut
			  && (strcmp(&sp->chars[ap->tt_desigidx],
				     &sp->chars[bp->tt_desigidx])
			      == 0));
	}
	return result;
}

static const int	mon_lengths[2][MONSPERYEAR] = {
	{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
	{ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

static const int	year_lengths[2] = {
	DAYSPERNYEAR, DAYSPERLYEAR
};

/*
** Given a pointer into a timezone string, scan until a character that is not
** a valid character in a time zone abbreviation is found.
** Return a pointer to that character.
*/

static ATTRIBUTE_PURE const char *
getzname(const char *strp)
{
	char	c;

	while ((c = *strp) != '\0' && !is_digit(c) && c != ',' && c != '-' &&
		c != '+')
			++strp;
	return strp;
}

/*
** Given a pointer into an extended timezone string, scan until the ending
** delimiter of the time zone abbreviation is located.
** Return a pointer to the delimiter.
**
** As with getzname above, the legal character set is actually quite
** restricted, with other characters producing undefined results.
** We don't do any checking here; checking is done later in common-case code.
*/

static ATTRIBUTE_PURE const char *
getqzname(const char *strp, const int delim)
{
	int	c;

	while ((c = *strp) != '\0' && c != delim)
		++strp;
	return strp;
}

/*
** Given a pointer into a timezone string, extract a number from that string.
** Check that the number is within a specified range; if it is not, return
** NULL.
** Otherwise, return a pointer to the first character not part of the number.
*/

static const char *
getnum(const char *strp, int *const nump, const int min, const int max)
{
	char	c;
	int	num;

	if (strp == NULL || !is_digit(c = *strp)) {
		errno = EINVAL;
		return NULL;
	}
	num = 0;
	do {
		num = num * 10 + (c - '0');
		if (num > max) {
			errno = EOVERFLOW;
			return NULL;	/* illegal value */
		}
		c = *++strp;
	} while (is_digit(c));
	if (num < min) {
		errno = EINVAL;
		return NULL;		/* illegal value */
	}
	*nump = num;
	return strp;
}

/*
** Given a pointer into a timezone string, extract a number of seconds,
** in hh[:mm[:ss]] form, from the string.
** If any error occurs, return NULL.
** Otherwise, return a pointer to the first character not part of the number
** of seconds.
*/

static const char *
getsecs(const char *strp, int_fast32_t *const secsp)
{
	int	num;

	/*
	** 'HOURSPERDAY * DAYSPERWEEK - 1' allows quasi-Posix rules like
	** "M10.4.6/26", which does not conform to Posix,
	** but which specifies the equivalent of
	** "02:00 on the first Sunday on or after 23 Oct".
	*/
	strp = getnum(strp, &num, 0, HOURSPERDAY * DAYSPERWEEK - 1);
	if (strp == NULL)
		return NULL;
	*secsp = num * (int_fast32_t) SECSPERHOUR;
	if (*strp == ':') {
		++strp;
		strp = getnum(strp, &num, 0, MINSPERHOUR - 1);
		if (strp == NULL)
			return NULL;
		*secsp += num * SECSPERMIN;
		if (*strp == ':') {
			++strp;
			/* 'SECSPERMIN' allows for leap seconds.  */
			strp = getnum(strp, &num, 0, SECSPERMIN);
			if (strp == NULL)
				return NULL;
			*secsp += num;
		}
	}
	return strp;
}

/*
** Given a pointer into a timezone string, extract an offset, in
** [+-]hh[:mm[:ss]] form, from the string.
** If any error occurs, return NULL.
** Otherwise, return a pointer to the first character not part of the time.
*/

static const char *
getoffset(const char *strp, int_fast32_t *const offsetp)
{
	bool neg = false;

	if (*strp == '-') {
		neg = true;
		++strp;
	} else if (*strp == '+')
		++strp;
	strp = getsecs(strp, offsetp);
	if (strp == NULL)
		return NULL;		/* illegal time */
	if (neg)
		*offsetp = -*offsetp;
	return strp;
}

/*
** Given a pointer into a timezone string, extract a rule in the form
** date[/time]. See POSIX section 8 for the format of "date" and "time".
** If a valid rule is not found, return NULL.
** Otherwise, return a pointer to the first character not part of the rule.
*/

static const char *
getrule(const char *strp, struct rule *const rulep)
{
	if (*strp == 'J') {
		/*
		** Julian day.
		*/
		rulep->r_type = JULIAN_DAY;
		++strp;
		strp = getnum(strp, &rulep->r_day, 1, DAYSPERNYEAR);
	} else if (*strp == 'M') {
		/*
		** Month, week, day.
		*/
		rulep->r_type = MONTH_NTH_DAY_OF_WEEK;
		++strp;
		strp = getnum(strp, &rulep->r_mon, 1, MONSPERYEAR);
		if (strp == NULL)
			return NULL;
		if (*strp++ != '.')
			return NULL;
		strp = getnum(strp, &rulep->r_week, 1, 5);
		if (strp == NULL)
			return NULL;
		if (*strp++ != '.')
			return NULL;
		strp = getnum(strp, &rulep->r_day, 0, DAYSPERWEEK - 1);
	} else if (is_digit(*strp)) {
		/*
		** Day of year.
		*/
		rulep->r_type = DAY_OF_YEAR;
		strp = getnum(strp, &rulep->r_day, 0, DAYSPERLYEAR - 1);
	} else	return NULL;		/* invalid format */
	if (strp == NULL)
		return NULL;
	if (*strp == '/') {
		/*
		** Time specified.
		*/
		++strp;
		strp = getoffset(strp, &rulep->r_time);
	} else	rulep->r_time = 2 * SECSPERHOUR;	/* default = 2:00:00 */
	return strp;
}

/*
** Given a year, a rule, and the offset from UT at the time that rule takes
** effect, calculate the year-relative time that rule takes effect.
*/

static int_fast32_t
transtime(const int year, const struct rule *const rulep,
	  const int_fast32_t offset)
{
	bool	leapyear;
	int_fast32_t value;
	int	i;
	int		d, m1, yy0, yy1, yy2, dow;

	INITIALIZE(value);
	leapyear = isleap(year);
	switch (rulep->r_type) {

	case JULIAN_DAY:
		/*
		** Jn - Julian day, 1 == January 1, 60 == March 1 even in leap
		** years.
		** In non-leap years, or if the day number is 59 or less, just
		** add SECSPERDAY times the day number-1 to the time of
		** January 1, midnight, to get the day.
		*/
		value = (rulep->r_day - 1) * SECSPERDAY;
		if (leapyear && rulep->r_day >= 60)
			value += SECSPERDAY;
		break;

	case DAY_OF_YEAR:
		/*
		** n - day of year.
		** Just add SECSPERDAY times the day number to the time of
		** January 1, midnight, to get the day.
		*/
		value = rulep->r_day * SECSPERDAY;
		break;

	case MONTH_NTH_DAY_OF_WEEK:
		/*
		** Mm.n.d - nth "dth day" of month m.
		*/

		/*
		** Use Zeller's Congruence to get day-of-week of first day of
		** month.
		*/
		m1 = (rulep->r_mon + 9) % 12 + 1;
		yy0 = (rulep->r_mon <= 2) ? (year - 1) : year;
		yy1 = yy0 / 100;
		yy2 = yy0 % 100;
		dow = ((26 * m1 - 2) / 10 +
			1 + yy2 + yy2 / 4 + yy1 / 4 - 2 * yy1) % 7;
		if (dow < 0)
			dow += DAYSPERWEEK;

		/*
		** "dow" is the day-of-week of the first day of the month. Get
		** the day-of-month (zero-origin) of the first "dow" day of the
		** month.
		*/
		d = rulep->r_day - dow;
		if (d < 0)
			d += DAYSPERWEEK;
		for (i = 1; i < rulep->r_week; ++i) {
			if (d + DAYSPERWEEK >=
				mon_lengths[leapyear][rulep->r_mon - 1])
					break;
			d += DAYSPERWEEK;
		}

		/*
		** "d" is the day-of-month (zero-origin) of the day we want.
		*/
		value = d * SECSPERDAY;
		for (i = 0; i < rulep->r_mon - 1; ++i)
			value += mon_lengths[leapyear][i] * SECSPERDAY;
		break;
	}

	/*
	** "value" is the year-relative time of 00:00:00 UT on the day in
	** question. To get the year-relative time of the specified local
	** time on that day, add the transition time and the current offset
	** from UT.
	*/
	return value + rulep->r_time + offset;
}

/*
** Given a POSIX section 8-style TZ string, fill in the rule tables as
** appropriate.
*/

static bool
tzparse(const char *name, struct state *sp, bool lastditch)
{
	const char *	stdname;
	const char *	dstname;
	size_t		stdlen;
	size_t		dstlen;
	size_t		charcnt;
	int_fast32_t	stdoffset;
	int_fast32_t	dstoffset;
	char *		cp;
	bool		load_ok;

	dstname = NULL; /* XXX gcc */
	stdname = name;
	if (lastditch) {
		stdlen = sizeof "GMT" - 1;
		name += stdlen;
		stdoffset = 0;
	} else {
		if (*name == '<') {
			name++;
			stdname = name;
			name = getqzname(name, '>');
			if (*name != '>')
			  return false;
			stdlen = name - stdname;
			name++;
		} else {
			name = getzname(name);
			stdlen = name - stdname;
		}
		if (!stdlen)
			return false;
		name = getoffset(name, &stdoffset);
		if (name == NULL)
			return false;
	}
	charcnt = stdlen + 1;
	if (sizeof sp->chars < charcnt)
		return false;
	load_ok = tzload(TZDEFRULES, sp, false) == 0;
	if (!load_ok)
		sp->leapcnt = 0;		/* so, we're off a little */
	if (*name != '\0') {
		if (*name == '<') {
			dstname = ++name;
			name = getqzname(name, '>');
			if (*name != '>')
				return false;
			dstlen = name - dstname;
			name++;
		} else {
			dstname = name;
			name = getzname(name);
			dstlen = name - dstname; /* length of DST abbr. */
		}
		if (!dstlen)
		  return false;
		charcnt += dstlen + 1;
		if (sizeof sp->chars < charcnt)
		  return false;
		if (*name != '\0' && *name != ',' && *name != ';') {
			name = getoffset(name, &dstoffset);
			if (name == NULL)
			  return false;
		} else	dstoffset = stdoffset - SECSPERHOUR;
		if (*name == '\0' && !load_ok)
			name = TZDEFRULESTRING;
		if (*name == ',' || *name == ';') {
			struct rule	start;
			struct rule	end;
			int		year;
			int		yearlim;
			int		timecnt;
			time_t		janfirst;
			int_fast32_t janoffset = 0;
			int yearbeg;

			++name;
			if ((name = getrule(name, &start)) == NULL)
				return false;
			if (*name++ != ',')
				return false;
			if ((name = getrule(name, &end)) == NULL)
				return false;
			if (*name != '\0')
				return false;
			sp->typecnt = 2;	/* standard time and DST */
			/*
			** Two transitions per year, from EPOCH_YEAR forward.
			*/
			init_ttinfo(&sp->ttis[0], -stdoffset, false, 0);
			init_ttinfo(&sp->ttis[1], -dstoffset, true, 
			    (int)(stdlen + 1));
			sp->defaulttype = 0;
			timecnt = 0;
			janfirst = 0;
			yearbeg = EPOCH_YEAR;

			do {
			  int_fast32_t yearsecs
			    = year_lengths[isleap(yearbeg - 1)] * SECSPERDAY;
			  yearbeg--;
			  if (increment_overflow_time(&janfirst, -yearsecs)) {
			    janoffset = -yearsecs;
			    break;
			  }
			} while (EPOCH_YEAR - YEARSPERREPEAT / 2 < yearbeg);

			yearlim = yearbeg + YEARSPERREPEAT + 1;
			for (year = yearbeg; year < yearlim; year++) {
				int_fast32_t
				  starttime = transtime(year, &start, stdoffset),
				  endtime = transtime(year, &end, dstoffset);
				int_fast32_t
				  yearsecs = (year_lengths[isleap(year)]
					      * SECSPERDAY);
				bool reversed = endtime < starttime;
				if (reversed) {
					int_fast32_t swap = starttime;
					starttime = endtime;
					endtime = swap;
				}
				if (reversed
				    || (starttime < endtime
					&& (endtime - starttime
					    < (yearsecs
					       + (stdoffset - dstoffset))))) {
					if (TZ_MAX_TIMES - 2 < timecnt)
						break;
					sp->ats[timecnt] = janfirst;
					if (! increment_overflow_time
					    (&sp->ats[timecnt],
					     janoffset + starttime))
					  sp->types[timecnt++] = !reversed;
					sp->ats[timecnt] = janfirst;
					if (! increment_overflow_time
					    (&sp->ats[timecnt],
					     janoffset + endtime)) {
					  sp->types[timecnt++] = reversed;
					  yearlim = year + YEARSPERREPEAT + 1;
					}
				}
				if (increment_overflow_time
				    (&janfirst, janoffset + yearsecs))
					break;
				janoffset = 0;
			}
			sp->timecnt = timecnt;
			if (! timecnt) {
				sp->ttis[0] = sp->ttis[1];
				sp->typecnt = 1;	/* Perpetual DST.  */
			} else if (YEARSPERREPEAT < year - yearbeg)
				sp->goback = sp->goahead = true;
		} else {
			int_fast32_t	theirstdoffset;
			int_fast32_t	theirdstoffset;
			int_fast32_t	theiroffset;
			bool		isdst;
			int		i;
			int		j;

			if (*name != '\0')
				return false;
			/*
			** Initial values of theirstdoffset and theirdstoffset.
			*/
			theirstdoffset = 0;
			for (i = 0; i < sp->timecnt; ++i) {
				j = sp->types[i];
				if (!sp->ttis[j].tt_isdst) {
					theirstdoffset =
						- sp->ttis[j].tt_utoff;
					break;
				}
			}
			theirdstoffset = 0;
			for (i = 0; i < sp->timecnt; ++i) {
				j = sp->types[i];
				if (sp->ttis[j].tt_isdst) {
					theirdstoffset =
						- sp->ttis[j].tt_utoff;
					break;
				}
			}
			/*
			** Initially we're assumed to be in standard time.
			*/
			isdst = false;
			theiroffset = theirstdoffset;
			/*
			** Now juggle transition times and types
			** tracking offsets as you do.
			*/
			for (i = 0; i < sp->timecnt; ++i) {
				j = sp->types[i];
				sp->types[i] = sp->ttis[j].tt_isdst;
				if (sp->ttis[j].tt_ttisut) {
					/* No adjustment to transition time */
				} else {
					/*
					** If daylight saving time is in
					** effect, and the transition time was
					** not specified as standard time, add
					** the daylight saving time offset to
					** the transition time; otherwise, add
					** the standard time offset to the
					** transition time.
					*/
					/*
					** Transitions from DST to DDST
					** will effectively disappear since
					** POSIX provides for only one DST
					** offset.
					*/
					if (isdst && !sp->ttis[j].tt_ttisstd) {
						sp->ats[i] += (time_t)
						    (dstoffset - theirdstoffset);
					} else {
						sp->ats[i] += (time_t)
						    (stdoffset - theirstdoffset);
					}
				}
				theiroffset = -sp->ttis[j].tt_utoff;
				if (sp->ttis[j].tt_isdst)
					theirstdoffset = theiroffset;
				else	theirdstoffset = theiroffset;
			}
			/*
			** Finally, fill in ttis.
			*/
			init_ttinfo(&sp->ttis[0], -stdoffset, false, 0);
			init_ttinfo(&sp->ttis[1], -dstoffset, true,
			    (int)(stdlen + 1));
			sp->typecnt = 2;
			sp->defaulttype = 0;
		}
	} else {
		dstlen = 0;
		sp->typecnt = 1;		/* only standard time */
		sp->timecnt = 0;
		init_ttinfo(&sp->ttis[0], -stdoffset, false, 0);
		init_ttinfo(&sp->ttis[1], 0, false, 0);
		sp->defaulttype = 0;
	}
	sp->charcnt = (int)charcnt;
	cp = sp->chars;
	(void) memcpy(cp, stdname, stdlen);
	cp += stdlen;
	*cp++ = '\0';
	if (dstlen != 0) {
		(void) memcpy(cp, dstname, dstlen);
		*(cp + dstlen) = '\0';
	}
	return true;
}

#if 0
static int
zoneinit(struct state *sp, char const *name)
{
	if (name && ! name[0]) {
		/*
		** User wants it fast rather than right.
		*/
		sp->leapcnt = 0;		/* so, we're off a little */
		sp->timecnt = 0;
		sp->typecnt = 1;
		sp->charcnt = 0;
		sp->goback = sp->goahead = false;
		init_ttinfo(&sp->ttis[0], 0, false, 0);
		strcpy(sp->chars, gmt);
		sp->defaulttype = 0;
		return 0;
	} else {
		int err = tzload(name, sp, true);
		if (err != 0 && name && name[0] != ':' &&
		    tzparse(name, sp, false))
			err = 0;
		if (err == 0)
			scrub_abbrs(sp);
		return err;
	}
}
#endif

/*
** The easy way to behave "as if no library function calls" localtime
** is to not call it, so we drop its guts into "localsub", which can be
** freely called. (And no, the PANS doesn't require the above behavior,
** but it *is* desirable.)
**
** If successful and SETNAME is nonzero,
** set the applicable parts of tzname, timezone and altzone;
** however, it's OK to omit this step if the timezone is POSIX-compatible,
** since in that case tzset should have already done this step correctly.
** SETNAME's type is intfast32_t for compatibility with gmtsub,
** but it is actually a boolean and its value should be 0 or 1.
*/

/*ARGSUSED*/
static struct tm *
localsub(struct state const *sp, time_t const *timep, int_fast32_t setname,
	 struct tm *const tmp)
{
	const struct _ttinfo *	ttisp;
	int			i;
	struct tm *		result;
	const time_t			t = *timep;

#if 0
	if (sp == NULL) {
		/* Don't bother to set tzname etc.; tzset has already done it.  */
		return gmtsub(gmtptr, timep, 0, tmp);
	}
#endif
	if ((sp->goback && t < sp->ats[0]) ||
		(sp->goahead && t > sp->ats[sp->timecnt - 1])) {
			time_t			newt = t;
			time_t		seconds;
			time_t		years;

			if (t < sp->ats[0])
				seconds = sp->ats[0] - t;
			else	seconds = t - sp->ats[sp->timecnt - 1];
			--seconds;
			years = (time_t)((seconds / SECSPERREPEAT + 1) * YEARSPERREPEAT);
			seconds = (time_t)(years * AVGSECSPERYEAR);
			if (t < sp->ats[0])
				newt += seconds;
			else	newt -= seconds;
			if (newt < sp->ats[0] ||
				newt > sp->ats[sp->timecnt - 1]) {
				errno = EINVAL;
				return NULL;	/* "cannot happen" */
			}
			result = localsub(sp, &newt, setname, tmp);
			if (result) {
				int_fast64_t newy;

				newy = result->tm_year;
				if (t < sp->ats[0])
					newy -= years;
				else	newy += years;
				if (! (INT_MIN <= newy && newy <= INT_MAX)) {
					errno = EOVERFLOW;
					return NULL;
				}
				result->tm_year = (int)newy;
			}
			return result;
	}
	if (sp->timecnt == 0 || t < sp->ats[0]) {
		i = sp->defaulttype;
	} else {
		int	lo = 1;
		int	hi = sp->timecnt;

		while (lo < hi) {
			int	mid = (lo + hi) / 2;

			if (t < sp->ats[mid])
				hi = mid;
			else	lo = mid + 1;
		}
		i = (int) sp->types[lo - 1];
	}
	ttisp = &sp->ttis[i];
	/*
	** To get (wrong) behavior that's compatible with System V Release 2.0
	** you'd replace the statement below with
	**	t += ttisp->tt_utoff;
	**	timesub(&t, 0L, sp, tmp);
	*/
	result = timesub(&t, ttisp->tt_utoff, sp, tmp);
	if (result) {
		result->tm_isdst = ttisp->tt_isdst;
#ifdef TM_ZONE
		result->TM_ZONE = __UNCONST(&sp->chars[ttisp->tt_desigidx]);
#endif /* defined TM_ZONE */
	}
	return result;
}



/*
** Return the number of leap years through the end of the given year
** where, to make the math easy, the answer for year zero is defined as zero.
*/
static int
leaps_thru_end_of_nonneg(int y)
{
	return y / 4 - y / 100 + y / 400;
}

static int ATTRIBUTE_PURE
leaps_thru_end_of(const int y)
{
	return (y < 0
		? -1 - leaps_thru_end_of_nonneg(-1 - y)
		: leaps_thru_end_of_nonneg(y));
}

static struct tm *
timesub(const time_t *timep, int_fast32_t offset,
    const struct state *sp, struct tm *tmp)
{
	const struct lsinfo *	lp;
	time_t			tdays;
	int			idays;	/* unsigned would be so 2003 */
	int_fast64_t		rem;
	int			y;
	const int *		ip;
	int_fast64_t		corr;
	int			hit;
	int			i;

	corr = 0;
	hit = false;
	i = (sp == NULL) ? 0 : sp->leapcnt;
	while (--i >= 0) {
		lp = &sp->lsis[i];
		if (*timep >= lp->ls_trans) {
			corr = lp->ls_corr;
			hit = (*timep == lp->ls_trans
			       && (i == 0 ? 0 : lp[-1].ls_corr) < corr);
			break;
		}
	}
	y = EPOCH_YEAR;
	tdays = (time_t)(*timep / SECSPERDAY);
	rem = *timep % SECSPERDAY;
	while (tdays < 0 || tdays >= year_lengths[isleap(y)]) {
		int		newy;
		time_t	tdelta;
		int	idelta;
		int	leapdays;

		tdelta = tdays / DAYSPERLYEAR;
		if (! ((! TYPE_SIGNED(time_t) || INT_MIN <= tdelta)
		       && tdelta <= INT_MAX))
			goto out_of_range;
		_DIAGASSERT(__type_fit(int, tdelta));
		idelta = (int)tdelta;
		if (idelta == 0)
			idelta = (tdays < 0) ? -1 : 1;
		newy = y;
		if (increment_overflow(&newy, idelta))
			goto out_of_range;
		leapdays = leaps_thru_end_of(newy - 1) -
			leaps_thru_end_of(y - 1);
		tdays -= ((time_t) newy - y) * DAYSPERNYEAR;
		tdays -= leapdays;
		y = newy;
	}
	/*
	** Given the range, we can now fearlessly cast...
	*/
	idays = (int) tdays;
	rem += offset - corr;
	while (rem < 0) {
		rem += SECSPERDAY;
		--idays;
	}
	while (rem >= SECSPERDAY) {
		rem -= SECSPERDAY;
		++idays;
	}
	while (idays < 0) {
		if (increment_overflow(&y, -1))
			goto out_of_range;
		idays += year_lengths[isleap(y)];
	}
	while (idays >= year_lengths[isleap(y)]) {
		idays -= year_lengths[isleap(y)];
		if (increment_overflow(&y, 1))
			goto out_of_range;
	}
	tmp->tm_year = y;
	if (increment_overflow(&tmp->tm_year, -TM_YEAR_BASE))
		goto out_of_range;
	tmp->tm_yday = idays;
	/*
	** The "extra" mods below avoid overflow problems.
	*/
	tmp->tm_wday = EPOCH_WDAY +
		((y - EPOCH_YEAR) % DAYSPERWEEK) *
		(DAYSPERNYEAR % DAYSPERWEEK) +
		leaps_thru_end_of(y - 1) -
		leaps_thru_end_of(EPOCH_YEAR - 1) +
		idays;
	tmp->tm_wday %= DAYSPERWEEK;
	if (tmp->tm_wday < 0)
		tmp->tm_wday += DAYSPERWEEK;
	tmp->tm_hour = (int) (rem / SECSPERHOUR);
	rem %= SECSPERHOUR;
	tmp->tm_min = (int) (rem / SECSPERMIN);
	/*
	** A positive leap second requires a special
	** representation. This uses "... ??:59:60" et seq.
	*/
	tmp->tm_sec = (int) (rem % SECSPERMIN) + hit;
	ip = mon_lengths[isleap(y)];
	for (tmp->tm_mon = 0; idays >= ip[tmp->tm_mon]; ++(tmp->tm_mon))
		idays -= ip[tmp->tm_mon];
	tmp->tm_mday = (int) (idays + 1);
	tmp->tm_isdst = 0;
#ifdef TM_GMTOFF
	tmp->TM_GMTOFF = offset;
#endif /* defined TM_GMTOFF */
	return tmp;
out_of_range:
	errno = EOVERFLOW;
	return NULL;
}



#ifndef WRONG
#define WRONG	((time_t)-1)
#endif /* !defined WRONG */

/*
** Normalize logic courtesy Paul Eggert.
*/

static bool
increment_overflow(int *ip, int j)
{
	int const	i = *ip;

	/*
	** If i >= 0 there can only be overflow if i + j > INT_MAX
	** or if j > INT_MAX - i; given i >= 0, INT_MAX - i cannot overflow.
	** If i < 0 there can only be overflow if i + j < INT_MIN
	** or if j < INT_MIN - i; given i < 0, INT_MIN - i cannot overflow.
	*/
	if ((i >= 0) ? (j > INT_MAX - i) : (j < INT_MIN - i))
		return true;
	*ip += j;
	return false;
}

static bool
increment_overflow_time(time_t *tp, int_fast32_t j)
{
	/*
	** This is like
	** 'if (! (TIME_T_MIN <= *tp + j && *tp + j <= TIME_T_MAX)) ...',
	** except that it does the right thing even if *tp + j would overflow.
	*/
	if (! (j < 0
	       ? (TYPE_SIGNED(time_t) ? TIME_T_MIN - j <= *tp : -1 - j < *tp)
	       : *tp <= TIME_T_MAX - j))
		return true;
	*tp += j;
	return false;
}

static int_fast64_t
leapcorr(struct state const *sp, time_t t)
{
	struct lsinfo const * lp;
	int		i;

	i = sp->leapcnt;
	while (--i >= 0) {
		lp = &sp->lsis[i];
		if (t >= lp->ls_trans)
			return lp->ls_corr;
	}
	return 0;
}

/* End localtime.c */

#if GS_USE_ICU == 1
static inline int
_NSToICUTZDisplayStyle(NSTimeZoneNameStyle style)
{
  switch (style)
    {
      case NSTimeZoneNameStyleStandard:
        return UCAL_STANDARD;
      case NSTimeZoneNameStyleShortStandard:
        return UCAL_SHORT_STANDARD;
      case NSTimeZoneNameStyleDaylightSaving:
        return UCAL_DST;
      case NSTimeZoneNameStyleShortDaylightSaving:
        return UCAL_SHORT_DST;
      default:
        return -1;
    }
}

static inline UCalendar *
ICUCalendarSetup (NSTimeZone *tz, NSLocale *locale)
{
  NSString *tzStr;
  int32_t tzLen;
  const char *cLocale;
  UChar tzName[BUFFER_SIZE];
  UCalendar *cal;
  UErrorCode err = U_ZERO_ERROR;
  
  tzStr = [tz name];
  if ((tzLen = [tzStr length]) > BUFFER_SIZE)
    tzLen = BUFFER_SIZE;
  [tzStr getCharacters: tzName range: NSMakeRange(0, tzLen)];
  cLocale = [[locale localeIdentifier] UTF8String];
  
  cal = ucal_open(tzName, tzLen, cLocale, UCAL_TRADITIONAL, &err);
  if (U_FAILURE(err))
    return NULL;
  
  return cal;
}
#endif

/* Possible location of system time zone files */
static NSString *tzdir = nil;

@class GSAbsTimeZone;
@class GSTimeZoneDetail;
@class GSAbsTimeZoneDetail;

@class GSPlaceholderTimeZone;

/*
 * Information for abstract placeholder class.
 */
static GSPlaceholderTimeZone	*defaultPlaceholderTimeZone;
static NSMapTable		*placeholderMap;
static GSAbsTimeZone            *commonAbsolutes[145] = { 0 };

/*
 * Temporary structure for holding time zone details.
 * This is the format in the data object.
 */
struct ttinfo
{
  char offset[4];         // Seconds east of UTC
  unsigned char isdst;    // Daylight savings time?
  unsigned char abbr_idx; // Index into time zone abbreviations string
} __attribute__((packed));

/*
 * And this is the structure used in the time zone instances.
 */
typedef struct {
  int32_t	offset;
  BOOL		isdst;
  unsigned char	abbr_idx;
  char		pad[2];
  NSString	*abbreviation;
} TypeInfo;

@interface	GSTimeZone : NSTimeZone
{
@public
  NSString	*timeZoneName;
  NSArray	*abbreviations;
  NSData	*timeZoneData;
  unsigned int	n_trans;
  unsigned int	n_types;
  int64_t	*trans;
  TypeInfo	*types;
  unsigned char	*idxs;
  struct state  sp;
}
@end

#if	defined(_WIN32)
@interface	GSWindowsTimeZone : NSTimeZone
{
@public
  NSString	*timeZoneName;
  NSString	*daylightZoneName;
  NSString	*timeZoneNameAbbr;
  NSString	*daylightZoneNameAbbr;
  LONG		Bias;
  LONG		StandardBias;
  LONG		DaylightBias;
  SYSTEMTIME	StandardDate;
  SYSTEMTIME	DaylightDate;
  struct state  sp;
}
@end
#endif


static NSTimeZone	*defaultTimeZone = nil;
static NSTimeZone	*localTimeZone = nil;
static NSTimeZone	*systemTimeZone = nil;

/* Dictionary for time zones.  Each time zone must have a unique
   name. */
static NSMutableDictionary *zoneDictionary;

/* one-to-one abbreviation to time zone name dictionary. */
static NSMutableDictionary *abbreviationDictionary = nil;
/* one-to-many abbreviation to time zone name dictionary. */
static NSMutableDictionary *abbreviationMap = nil;

/* Lock for creating time zones. */
static pthread_mutex_t zone_mutex;

static Class	NSTimeZoneClass;
static Class	GSPlaceholderTimeZoneClass;


/* Return path to a TimeZone directory file */
static NSString *_time_zone_path(NSString *subpath, NSString *type)
{
  NSBundle *gbundle;
  if (type == nil)
    type = @"";
  gbundle = [NSBundle bundleForClass: [NSObject class]];
  return [gbundle pathForResource: subpath
		           ofType: type
		      inDirectory: TIME_ZONE_DIR];
}

@interface GSPlaceholderTimeZone : NSTimeZone
@end

@interface GSAbsTimeZone : NSTimeZone
{
@public
  NSString	*name;
  id		detail;
  int		offset; // Offset from UTC in seconds.
}

- (id) initWithOffset: (NSInteger)anOffset name: (NSString*)aName;
@end

@interface NSLocalTimeZone : NSTimeZone
@end

@interface GSTimeZoneDetail : NSTimeZoneDetail
{
  NSTimeZone	*timeZone; // Time zone which created this object.
  NSString	*abbrev; // Abbreviation for time zone detail.
  int		offset; // Offset from UTC in seconds.
  BOOL		is_dst; // Is it daylight savings time?
}

- (id) initWithTimeZone: (NSTimeZone*)aZone
	     withAbbrev: (NSString*)anAbbrev
	     withOffset: (NSInteger)anOffset
		withDST: (BOOL)isDST;
@end

@interface GSAbsTimeZoneDetail : NSTimeZoneDetail
{
  GSAbsTimeZone	*zone; // Time zone which created this object.
}

- (id) initWithTimeZone: (GSAbsTimeZone*)aZone;
@end

/* Private methods for obtaining resource file names. */
@interface NSTimeZone (Private)
+ (NSString*) _getTimeZoneFile: (NSString*)name;
+ (void) _notified: (NSNotification*)n;
@end


@implementation GSPlaceholderTimeZone

- (id) autorelease
{
  NSWarnLog(@"-autorelease sent to uninitialised time zone");
  return self;		// placeholders never get released.
}

- (void) dealloc
{
  GSNOSUPERDEALLOC;	// placeholders never get deallocated.
}

- (id) initWithName: (NSString*)name data: (NSData*)data
{
  NSTimeZone	*zone;
  unsigned	length = [name length];

  if (length == 0)
    {
      NSLog(@"Disallowed null time zone name");
      return nil;
    }
  if (length == 15 && [name isEqual: @"NSLocalTimeZone"])
    {
      zone = RETAIN(localTimeZone);
      DESTROY(self);
      return (GSPlaceholderTimeZone*)zone;
    }

  /*
   * Return a cached time zone if possible.
   * NB. if data of cached zone does not match new data ... don't use cache
   */
  pthread_mutex_lock(&zone_mutex);
  zone = [zoneDictionary objectForKey: name];
  if (data != nil && [data isEqual: [zone data]] == NO)
    {
      zone = nil;
    }
  IF_NO_GC(RETAIN(zone));
  pthread_mutex_unlock(&zone_mutex);

  if (zone == nil)
    {
        int		err;
	const char	*zoneName = [name UTF8String];
        
#if	defined(_WIN32)
	GSWindowsTimeZone *newZone = [GSWindowsTimeZone alloc];
#else
	GSTimeZone *newZone = [GSTimeZone alloc];
#endif

	struct state *sp = &newZone->sp;
	int i;

	err = tzload(zoneName, sp, true);
	if (err != 0 && zoneName && zoneName[0] != ':' &&
	    tzparse(zoneName, sp, false))
		err = 0;
	if (err == 0)
		scrub_abbrs(sp);
	else
                return nil;

	
	newZone->timeZoneName = [NSString stringWithCString: zoneName];

        id abbrs[sp->typecnt];
	for (i = 0; i < sp->typecnt; i++) {
		const struct _ttinfo * const ttisp = &sp->ttis[i];
		char *abbr = &sp->chars[ttisp->tt_desigidx];
		abbrs[i] = [[NSString alloc] initWithUTF8String: abbr];
	}
	newZone->abbreviations =
	    [[NSArray alloc] initWithObjects: abbrs count: sp->typecnt];  

	newZone->timeZoneData = sp->data;
	newZone->n_trans = sp->timecnt;
	newZone->n_types = sp->typecnt;

	size_t translen = sp->timecnt * sizeof(int64_t *);
      	void *transbuf = NSZoneMalloc(NSDefaultMallocZone(), translen);
	newZone->trans = (int64_t *)transbuf;
	for (i = 0; i < sp->timecnt ; i++)
		newZone->trans[i] = sp->ats[i];

	size_t typelen = sp->typecnt * sizeof(TypeInfo);
      	void *typebuf = NSZoneMalloc(NSDefaultMallocZone(), typelen);
	newZone->types = (TypeInfo *)typebuf;
	for (i = 0; i < sp->typecnt; i++) {
		const struct _ttinfo *const ttisp = &sp->ttis[i];
		const char *abbr = &sp->chars[ttisp->tt_desigidx];
		newZone->types[i].offset = ttisp->tt_utoff;
		newZone->types[i].isdst = ttisp->tt_isdst;
		newZone->types[i].abbr_idx = ttisp->tt_desigidx;
		newZone->types[i].abbreviation = 
		    [NSString stringWithUTF8String: abbr];
	}

	size_t idxlen = sp->timecnt * sizeof(unsigned char);
      	void *idxbuf = NSZoneMalloc(NSDefaultMallocZone(), idxlen);
	newZone->idxs = (unsigned char *)idxbuf;
	for (i = 0; i < sp->timecnt; i++) {
		newZone->idxs[i] = sp->types[i];
	}

	zone = newZone;
    }
  DESTROY(self);
  return (GSPlaceholderTimeZone*)zone;
}

- (oneway void) release
{
  return;		// placeholders never get released.
}

- (id) retain
{
  return self;		// placeholders never get retained.
}
@end



@implementation	NSLocalTimeZone

- (NSString*) abbreviation
{
  return [[NSTimeZoneClass defaultTimeZone] abbreviation];
}

- (NSString*) abbreviationForDate: (NSDate*)aDate
{
  return [[NSTimeZoneClass defaultTimeZone] abbreviationForDate: aDate];
}

- (id) autorelease
{
  return self;
}

- (id) copy
{
  return self;
}

- (id) copyWithZone: (NSZone*)z
{
  return self;
}

- (NSData*) data
{
  return [[NSTimeZoneClass defaultTimeZone] data];
}

- (void) encodeWithCoder: (NSCoder*)aCoder
{
  [aCoder encodeObject: @"NSLocalTimeZone"];
}

- (id) init
{
  return self;
}

- (BOOL) isDaylightSavingTime
{
  return [[NSTimeZoneClass defaultTimeZone] isDaylightSavingTime];
}

- (BOOL) isDaylightSavingTimeForDate: (NSDate*)aDate
{
  return [[NSTimeZoneClass defaultTimeZone] isDaylightSavingTimeForDate: aDate];
}

- (NSString*) name
{
  return [[NSTimeZoneClass defaultTimeZone] name];
}

- (oneway void) release
{
}

- (id) retain
{
  return self;
}

- (NSInteger) secondsFromGMT
{
  return [[NSTimeZoneClass defaultTimeZone] secondsFromGMT];
}

- (NSInteger) secondsFromGMTForDate: (NSDate*)aDate
{
  return [[NSTimeZoneClass defaultTimeZone] secondsFromGMTForDate: aDate];
}

- (NSArray*) timeZoneDetailArray
{
  return [[NSTimeZoneClass defaultTimeZone] timeZoneDetailArray];
}

- (NSTimeZoneDetail*) timeZoneDetailForDate: (NSDate*)date
{
  return [[NSTimeZoneClass defaultTimeZone] timeZoneDetailForDate: date];
}

- (NSString*) timeZoneName
{
  return [[NSTimeZoneClass defaultTimeZone] timeZoneName];
}

@end


@implementation GSAbsTimeZone

static int		uninitialisedOffset = 100000;
static NSMapTable	*absolutes = 0;

+ (void) initialize
{
  if (self == [GSAbsTimeZone class])
    {
      absolutes = NSCreateMapTable(NSIntegerMapKeyCallBacks,
	NSNonOwnedPointerMapValueCallBacks, 0);
      [[NSObject leakAt: (id*)&absolutes] release];
    }
}

- (NSString*) abbreviationForDate: (NSDate*)aDate
{
  return name;
}

- (void) dealloc
{
  if (offset != uninitialisedOffset)
    {
      pthread_mutex_lock(&zone_mutex);
      NSMapRemove(absolutes, (void*)(uintptr_t)offset);
      pthread_mutex_unlock(&zone_mutex);
    }
  RELEASE(name);
  RELEASE(detail);
  [super dealloc];
}

- (void) encodeWithCoder: (NSCoder*)aCoder
{
  [aCoder encodeObject: name];
}

- (id) initWithOffset: (NSInteger)anOffset name: (NSString*)aName
{
  GSAbsTimeZone	*z;
  int		extra;
  int		sign = anOffset >= 0 ? 1 : -1;

  /*
   * Set the uninitialised offset so that dealloc before full
   * initialisation won't remove the timezeone for offset 0 from cache.
   */
  offset = uninitialisedOffset;

  /*
   * Round the offset to the nearest minute, (for MacOS-X compatibility)
   * and ensure it is no more than 18 hours.
   */
  anOffset *= sign;
  extra = anOffset % 60;
  if (extra < 30)
    {
      anOffset -= extra;
    }
  else
    {
      anOffset += 60 - extra;
    }
  if (anOffset > 64800)
    {
      DESTROY(self);
      return nil;
    }
  anOffset *= sign;

  if (anOffset % 900 == 0)
    {
      z = commonAbsolutes[anOffset/900 + 72];
      if (z != nil)
        {
          IF_NO_GC(RETAIN(z));
          DESTROY(self);
          return z;
        }
    }

  pthread_mutex_lock(&zone_mutex);
  z = (GSAbsTimeZone*)NSMapGet(absolutes, (void*)(uintptr_t)anOffset);
  if (z != nil)
    {
      IF_NO_GC(RETAIN(z));
      DESTROY(self);
    }
  else
    {
      if (aName == nil)
	{
	  if (anOffset % 60 == 0)
	    {
	      char	s = (anOffset >= 0) ? '+' : '-';
	      unsigned	i = (anOffset >= 0) ? anOffset / 60 : -anOffset / 60;
	      unsigned	h = (i / 60) % 24;
	      unsigned	m = i % 60;
	      char	buf[9];

	      snprintf(buf, sizeof(buf), "GMT%c%02u%02u", s, h, m);
	      name = [[NSString alloc] initWithUTF8String: buf];
	    }
	  else
	    {
	      /*
	       * Should never happen now we round to the minute
	       * for MacOS-X compatibnility.
	       */
	      name = [[NSString alloc]
		initWithFormat: @"NSAbsoluteTimeZone:%"PRIdPTR, anOffset];
	    }
	}
      else
	{
	  name = [aName copy];
	}
      detail = [[GSAbsTimeZoneDetail alloc] initWithTimeZone: self];
      offset = anOffset;
      z = self;
      NSMapInsert(absolutes, (void*)(uintptr_t)anOffset, (void*)z);
      [zoneDictionary setObject: self forKey: (NSString*)name];
    }
  if (anOffset % 900 == 0)
    {
      int       index = anOffset/900 + 72;

      if (nil == commonAbsolutes[index])
        {
          commonAbsolutes[index] = RETAIN(self);
        }
    }
  pthread_mutex_unlock(&zone_mutex);
  return z;
}

- (BOOL) isDaylightSavingTimeZoneForDate: (NSDate*)aDate
{
  return NO;
}

- (NSString*) name
{
  return name;
}

- (NSInteger) secondsFromGMTForDate: (NSDate*)aDate
{
  return offset;
}

- (NSArray*) timeZoneDetailArray
{
  return [NSArray arrayWithObject: detail];
}

- (NSTimeZoneDetail*) timeZoneDetailForDate: (NSDate*)date
{
  return detail;
}

- (NSString*) timeZoneName
{
  return name;
}
@end


@implementation GSTimeZoneDetail

- (void) dealloc
{
  RELEASE(timeZone);
  [super dealloc];
}

- (id) initWithCoder: (NSCoder*)aDecoder
{
  [aDecoder decodeValueOfObjCType: @encode(id) at: &abbrev];
  [aDecoder decodeValueOfObjCType: @encode(int) at: &offset];
  [aDecoder decodeValueOfObjCType: @encode(BOOL) at: &is_dst];
  return self;
}

- (id) initWithTimeZone: (NSTimeZone*)aZone
	     withAbbrev: (NSString*)anAbbrev
	     withOffset: (NSInteger)anOffset
		withDST: (BOOL)isDST
{
  timeZone = RETAIN(aZone);
  abbrev = anAbbrev;		// NB. Depend on this being retained in aZone
  offset = anOffset;
  is_dst = isDST;
  return self;
}

- (BOOL) isDaylightSavingTimeZone
{
  return is_dst;
}

- (NSString*) name
{
  return [timeZone name];
}

- (NSString*) timeZoneAbbreviation
{
  return abbrev;
}

- (NSArray*) timeZoneDetailArray
{
  return [timeZone timeZoneDetailArray];
}

- (NSTimeZoneDetail*) timeZoneDetailForDate: (NSDate*)date
{
  return [timeZone timeZoneDetailForDate: date];
}

- (NSInteger) timeZoneSecondsFromGMT
{
  return offset;
}

- (NSInteger) timeZoneSecondsFromGMTForDate: (NSDate*)aDate
{
  return offset;
}

@end


@implementation GSAbsTimeZoneDetail

- (NSString*) abbreviation
{
  return zone->name;
}

- (NSString*) abbreviationForDate: (NSDate*)aDate
{
  return zone->name;
}

- (void) dealloc
{
  RELEASE(zone);
  [super dealloc];
}

- (id) initWithTimeZone: (GSAbsTimeZone*)aZone
{
  zone = RETAIN(aZone);
  return self;
}

- (BOOL) isDaylightSavingTimeZone
{
  return NO;
}

- (BOOL) isDaylightSavingTimeZoneForDate: (NSDate*)aDate
{
  return NO;
}

- (NSString*) name
{
  return zone->name;
}

- (NSString*) timeZoneAbbreviation
{
  return zone->name;
}

- (NSArray*) timeZoneDetailArray
{
  return [zone timeZoneDetailArray];
}

- (NSTimeZoneDetail*) timeZoneDetailForDate: (NSDate*)date
{
  return self;
}

- (NSInteger) timeZoneSecondsFromGMT
{
  return zone->offset;
}

- (NSInteger) timeZoneSecondsFromGMTForDate: (NSDate*)aDate
{
  return zone->offset;
}

@end


/**
 * <p>
 * The local time zone is obtained from, in order of preference:<br/ >
 *  1) the user defaults database: NSGlobalDomain "Local Time Zone"<br/ >
 *  2) the GNUSTEP_TZ environment variable<br/ >
 *  3) the file "localtime" in System/Library/Libraries/Resources/NSTimeZone<br/ >
 *  4) the TZ environment variable<br/ >
 *  5) The system zone settings (typically in /etc/localtime)<br/ >
 *  6) tzset and tznam on platforms which have it<br/ >
 *  7) Windows registry, on Win32 systems<br/ >
 *  8) or the fallback time zone (which is UTC)<br/ >
 * </p>
 * <p>If the GNUstep time zone datafiles become too out of date, one
 * can download an updated database from <uref
 * url="ftp://elsie.nci.nih.gov/pub/">ftp://elsie.nci.nih.gov/pub/</uref>
 * and compile it as specified in the README file in the
 * NSTimeZones directory.
 * </p>
 * <p>Time zone names in NSDates should be GMT, MET etc. not
 * Europe/Berlin, America/Washington etc.
 * </p>
 * <p>The problem with this is that various time zones may use the
 * same abbreviation (e.g. Australia/Brisbane and
 * America/New_York both use EST), and some time zones
 * may have different rules for daylight saving time even if the
 * abbreviation and offsets from UTC are the same.
 * </p>
 * <p>The problems with depending on the OS for providing time zone
 * info are that time zone names may vary
 * wildly between OSes (this could be a big problem when
 * archiving is used between different systems).
 * </p>
 * <p>Win32:  Time zone names read from the registry are different
 * from other GNUstep installations. Be careful when moving data
 * between platforms in this case.
 * </p>
 */
@implementation NSTimeZone

/**
 * Returns a dictionary containing time zone abbreviations and their
 * corresponding time zone names. More than one time zone may be associated
 * with a single abbreviation. In this case, the dictionary contains only
 * one (usually the most common) time zone name for the abbreviation.
 */
+ (NSDictionary*) abbreviationDictionary
{
  if (abbreviationDictionary != nil)
    {
      return abbreviationDictionary;
    }
  pthread_mutex_lock(&zone_mutex);
  if (abbreviationDictionary == nil)
    {
      NSAutoreleasePool	*pool = [NSAutoreleasePool new];
      NSString		*path;

      path = _time_zone_path (ABBREV_DICT, @"plist");
      if (path != nil)
	{
	  /*
	   * Fast mechanism ... load prebuilt data from file so we don't
	   * need to load in all time zones.
	   */
	  abbreviationDictionary
	    = RETAIN([[NSString stringWithContentsOfFile: path] propertyList]);
	}
      if (abbreviationDictionary == nil)
	{
	  NSMutableDictionary	*md;
	  NSString		*name;
	  NSEnumerator		*names;

	  /*
	   * Slow fallback ... load all time zones and generate
	   * abbreviation dictionary from them.
	   */
	  md = [[NSMutableDictionary alloc] init];
	  names = [[NSTimeZone knownTimeZoneNames] objectEnumerator];
	  while ((name = [names nextObject]) != nil)
	    {
	      NSTimeZone *zone;

	      if ((zone = [NSTimeZone timeZoneWithName: name]))
		{
		  NSEnumerator		*details;
		  NSTimeZoneDetail	*detail;
	
		  details = [[zone timeZoneDetailArray] objectEnumerator];
		  while ((detail = [details nextObject]) != nil)
		    {
		      [md setObject: name
			     forKey: [detail timeZoneAbbreviation]];
		    }
		}
	    }
          if ([md makeImmutable] == YES)
            {
              abbreviationDictionary = md;
            }
          else
            {
              abbreviationDictionary = [md copy];
              RELEASE(md);
            }
	}
      [pool drain];
    }
  pthread_mutex_unlock(&zone_mutex);
  return abbreviationDictionary;
}

/**
 * Returns a dictionary that maps abbreviations to the array
 * containing all the time zone names that use the abbreviation.
 */
+ (NSDictionary*) abbreviationMap
{
  /* Instead of creating the abbreviation dictionary when the class is
     initialized, we create it when we first need it, since the
     dictionary can be potentially very large, considering that it's
     almost never used. */
  if (abbreviationMap != nil)
    {
      return abbreviationMap;
    }
  pthread_mutex_lock(&zone_mutex);
  if (abbreviationMap == nil)
    {
      NSAutoreleasePool		*pool = [NSAutoreleasePool new];
      NSMutableDictionary	*md;
      NSMutableArray		*ma;
      NSString			*the_name;
      NSString			*the_abbrev;
      FILE			*file;
      char			abbrev[80];
      char			name[80];
      NSString			*path;

      /*
       * Read dictionary from file... fast mechanism because we don't have
       * to create all timezoneas and parse all their data files.
       */
      md = [NSMutableDictionary dictionaryWithCapacity: 100];
      path = _time_zone_path (ABBREV_MAP, nil);
      if (path != nil)
	{
#if	defined(_WIN32)
	  unichar	mode[3];

	  mode[0] = 'r';
	  mode[1] = 'b';
	  mode[2] = '\0';

	  file = _wfopen((const unichar*)[path fileSystemRepresentation], mode);
#else
	  file = fopen([path fileSystemRepresentation], "r");
#endif
	  if (file == NULL)
	    {
              pthread_mutex_unlock(&zone_mutex);
	      [NSException
		raise: NSInternalInconsistencyException
		format: @"Failed to open time zone abbreviation map."];
	    }
	  while (fscanf(file, "%79s %79s", abbrev, name) == 2)
	    {
	      the_name = [[NSString alloc] initWithUTF8String: name];
	      the_abbrev = [[NSString alloc] initWithUTF8String: abbrev];
	      ma = [md objectForKey: the_abbrev];
	      if (ma == nil)
		{
		  ma = [[NSMutableArray alloc] initWithCapacity: 1];
		  [md setObject: ma forKey: the_abbrev];
		  RELEASE(ma);
		}
	      RELEASE(the_abbrev);
	      if ([ma containsObject: the_name] == NO)
		{
		  [ma addObject: the_name];
		}
	      RELEASE(the_name);
	    }
	  fclose(file);
	}
      else
	{
	  NSString		*name;
	  NSEnumerator		*names;

	  /*
	   * Slow fallback mechanism ... go through all time names
	   * so we load all the time zone data and generate the info
	   * we need from it.
	   */
	  names = [[NSTimeZone knownTimeZoneNames] objectEnumerator];
	  while ((name = [names nextObject]) != nil)
	    {
	      NSTimeZone *zone;

	      if ((zone = [NSTimeZone timeZoneWithName: name]) != nil)
		{
		  NSEnumerator		*details;
		  NSTimeZoneDetail	*detail;
	
		  details = [[zone timeZoneDetailArray] objectEnumerator];
		  while ((detail = [details nextObject]) != nil)
		    {
		      the_abbrev = [detail timeZoneAbbreviation];
		      ma = [md objectForKey: the_abbrev];
		      if (ma == nil)
			{
			  ma = [[NSMutableArray alloc] initWithCapacity: 1];
			  [md setObject: ma forKey: the_abbrev];
			  RELEASE(ma);
			}
		      if ([ma containsObject: name] == NO)
		        {
		          [ma addObject: name];
			}
		    }
		}
	    }
	}

      /* Special case: Add the system time zone if
       * it doesn't exist in the map */
      the_abbrev = [systemTimeZone abbreviation];
      ma = [md objectForKey: the_abbrev];
      if (ma == nil)
	{
	  ma = [NSMutableArray new];
	  [md setObject: ma forKey: the_abbrev];
	  RELEASE(ma);
	}
      the_name = [systemTimeZone timeZoneName];
      if ([ma containsObject: the_name] == NO)
	{
	  [ma addObject: the_name];
	}

      if ([md makeImmutable] == YES)
        {
          abbreviationMap = RETAIN(md); 
        }
      else
        {
          abbreviationMap = [md copy];
        }
      [pool drain];
    }
  pthread_mutex_unlock(&zone_mutex);

  return abbreviationMap;
}

/**
 * Returns an array of all known time zone names.
 */
+ (NSArray*) knownTimeZoneNames
{
  static NSArray *namesArray = nil;

  /* We create the array only when we need it to reduce overhead. */
  if (namesArray != nil)
    {
      return namesArray;
    }

  pthread_mutex_lock(&zone_mutex);
  if (namesArray == nil)
    {
      unsigned		i;
      NSMutableArray	*ma;
      NSArray		*regionsArray;

      ma = [NSMutableArray new];
      regionsArray = [self timeZoneArray];

      for (i = 0; i < [regionsArray count]; i++)
	{
	  NSArray *names = [regionsArray objectAtIndex: i];

	  [ma addObjectsFromArray: names];
	}
      if ([ma makeImmutable] == YES)
        {
          namesArray = ma;
        }
      else
        {
          namesArray = [ma copy];
          RELEASE(ma);
        }
    }
  pthread_mutex_unlock(&zone_mutex);
  return namesArray;
}

+ (id) allocWithZone: (NSZone*)z
{
  if (self == NSTimeZoneClass)
    {
      /*
       * We return a placeholder object that can
       * be converted to a real object when its initialisation method
       * is called.
       */
      if (z == NSDefaultMallocZone() || z == 0)
	{
	  /*
	   * As a special case, we can return a placeholder for a time zone
	   * in the default zone extremely efficiently.
	   */
	  return defaultPlaceholderTimeZone;
	}
      else
	{
	  id	obj;

	  /*
	   * For anything other than the default zone, we need to
	   * locate the correct placeholder in the (lock protected)
	   * table of placeholders.
	   */
          pthread_mutex_lock(&zone_mutex);
	  obj = (id)NSMapGet(placeholderMap, (void*)z);
	  if (obj == nil)
	    {
	      /*
	       * There is no placeholder object for this zone, so we
	       * create a new one and use that.
	       */
	      obj = (id)NSAllocateObject(GSPlaceholderTimeZoneClass, 0, z);
	      NSMapInsert(placeholderMap, (void*)z, (void*)obj);
	    }
          pthread_mutex_unlock(&zone_mutex);
	  return obj;
	}
    }
  else
    {
      return NSAllocateObject(self, 0, z);
    }
}

/**
 * Return the default time zone for this process.
 */
+ (NSTimeZone*) defaultTimeZone
{
  NSTimeZone	*zone;

  pthread_mutex_lock(&zone_mutex);
  if (defaultTimeZone == nil)
    {
      zone = [self systemTimeZone];
    }
  else
    {
      zone = AUTORELEASE(RETAIN(defaultTimeZone));
    }
  pthread_mutex_unlock(&zone_mutex);
  return zone;
}

+ (void) initialize
{
  if (self == [NSTimeZone class])
    {
      NSTimeZoneClass = self;
      GS_INIT_RECURSIVE_MUTEX(zone_mutex);
      GSPlaceholderTimeZoneClass = [GSPlaceholderTimeZone class];
      zoneDictionary = [[NSMutableDictionary alloc] init];
      [[NSObject leakAt: &zoneDictionary] release];

      /*
       * Set up infrastructure for placeholder timezones.
       */
      defaultPlaceholderTimeZone = (GSPlaceholderTimeZone*)
	NSAllocateObject(GSPlaceholderTimeZoneClass, 0, NSDefaultMallocZone());
      [[NSObject leakAt: &defaultPlaceholderTimeZone] release];
      placeholderMap = NSCreateMapTable(NSNonOwnedPointerMapKeyCallBacks,
	NSNonRetainedObjectMapValueCallBacks, 0);
      [[NSObject leakAt: (id*)&placeholderMap] release];

      localTimeZone = [[NSLocalTimeZone alloc] init];
      [[NSObject leakAt: (id*)&localTimeZone] release];

      [[NSObject leakAt: (id*)&defaultTimeZone] release];
      [[NSObject leakAt: (id*)&systemTimeZone] release];
      [[NSObject leakAt: (id*)&abbreviationDictionary] release];
      [[NSObject leakAt: (id*)&abbreviationMap] release];
      [[NSObject leakAt: (id*)&absolutes] release];

      [[NSNotificationCenter defaultCenter] addObserver: self
        selector: @selector(_notified:)
        name: NSUserDefaultsDidChangeNotification
        object: nil];
    }
}

/**
 * Return a proxy to the default time zone for this process.
 */
+ (NSTimeZone*) localTimeZone
{
  return localTimeZone;
}

/**
 * Destroy the system time zone so that it will be recreated
 * next time it is used.
 */
+ (void) resetSystemTimeZone
{
  pthread_mutex_lock(&zone_mutex);
  DESTROY(systemTimeZone);
  pthread_mutex_unlock(&zone_mutex);
  [[NSNotificationCenter defaultCenter]
    postNotificationName: NSSystemTimeZoneDidChangeNotification
                  object: nil];
}

/**
 * Set the default time zone to be used for this process.
 */
+ (void) setDefaultTimeZone: (NSTimeZone*)aTimeZone
{
  if (aTimeZone != defaultTimeZone)
    {
      /*
       * We can't make the localTimeZone the default since that would
       * cause recursion ...
       */
      if (aTimeZone == localTimeZone)
	{
	  aTimeZone = [self systemTimeZone];
	}
      pthread_mutex_lock(&zone_mutex);
      ASSIGN(defaultTimeZone, aTimeZone);
      pthread_mutex_unlock(&zone_mutex);
    }
}

/**
 * Returns the current system time zone for the process.
 */
+ (NSTimeZone*) systemTimeZone
{
  NSTimeZone	*zone = nil;

  pthread_mutex_lock(&zone_mutex);
  if (systemTimeZone == nil)
    {
      NSFileManager *dflt = [NSFileManager defaultManager];
      NSString	*localZoneString = nil;
      NSString	*localZoneSource = nil;

      /*
       * setup default value in case something goes wrong.
       */
      systemTimeZone = RETAIN([NSTimeZoneClass timeZoneForSecondsFromGMT: 0]);

      /*
       * Try to get timezone from user defaults database
       */
      localZoneSource = [NSString stringWithFormat:
	@"NSUserDefaults: '%@'", LOCALDBKEY];
      localZoneString = [[NSUserDefaults standardUserDefaults]
	stringForKey: LOCALDBKEY];

      /*
       * Try to get timezone from GNUSTEP_TZ environment variable.
       */
      if (localZoneString == nil)
	{
          localZoneSource = _(@"environment variable: 'GNUSTEP_TZ'");
	  localZoneString = [[[NSProcessInfo processInfo]
	    environment] objectForKey: @"GNUSTEP_TZ"];
	}

      /*
       * Try to get timezone from LOCAL_TIME_FILE.
       */
      if (localZoneString == nil)
	{
	  NSString	*f = _time_zone_path(LOCAL_TIME_FILE, nil);

          localZoneSource = [NSString stringWithFormat: @"file: '%@'", f];
	  if (f != nil)
	    {
	      localZoneString = [NSString stringWithContentsOfFile: f];
	      localZoneString = [localZoneString stringByTrimmingSpaces];
	    }
	}

#if	defined(_WIN32)
      /*
       * Try to get timezone from windows system call.
       */
      {
      	TIME_ZONE_INFORMATION tz;
      	DWORD DST = GetTimeZoneInformation(&tz);

        localZoneSource = @"function: 'GetTimeZoneInformation()'";
      	if (DST == TIME_ZONE_ID_DAYLIGHT)
	  {
	    localZoneString = [NSString stringWithCharacters: tz.DaylightName
	      length: wcslen(tz.DaylightName)];
	  }
      	else
	  {
	    localZoneString = [NSString stringWithCharacters: tz.StandardName
	      length: wcslen(tz.StandardName)];
	  }
      }
#endif

      if (localZoneString == nil)
	{
	  if (YES == [dflt isReadableFileAtPath: @"/etc/timezone"])
	    {
	      NSString	*s;

	      s = [NSString stringWithContentsOfFile: @"/etc/timezone"];
	      s = [s stringByTrimmingSpaces];
	      if (0 != [s length])
		{
		  localZoneSource = _(@"/etc/timezone file");
		  localZoneString = s;
		}
	    }
	}

      if (localZoneString == nil)
	{
	  if (YES == [dflt isReadableFileAtPath: @"/etc/sysconfig/clock"])
	    {
	      NSString		*s;
	      NSEnumerator	*e;

	      s = [NSString stringWithContentsOfFile:
		@"/etc/sysconfig/clock"];
	      e = [[s componentsSeparatedByString: @"\n"] objectEnumerator];
	      while (nil != (s = [e nextObject]))
		{
		  s = [s stringByTrimmingSpaces];
                  // OpenSuse uses the non-standard key TIMEZONE
		  if ([s hasPrefix: @"ZONE"] || [s hasPrefix: @"TIMEZONE"])
		    {
                      if ([s hasPrefix: @"ZONE"])
                        s = [s substringFromIndex: 4];
                      else
                        s = [s substringFromIndex: 8];

		      s = [s stringByTrimmingSpaces];
		      if ([s hasPrefix: @"="])
			{
			  s = [s substringFromIndex: 1];
			  s = [s stringByTrimmingSpaces];
			  if ([s hasPrefix: @"\""])
			    {
			      s = [s substringFromIndex: 1];
			    }
			  if ([s hasSuffix: @"\""])
			    {
			      s = [s substringToIndex: [s length] - 1];
			    }
			  s = [s stringByTrimmingSpaces];
			  if ([s length] > 0)
			    {
			      localZoneSource = _(@"/etc/sysconfig/clock file");
			      localZoneString = s;
			    }
			}
		    }
		}
	    }
	}

      if (localZoneString == nil)
        {
          /* Get the zone name from the localtime file, assuming the file
	     is a symlink to the time zone. Getting the actual data (which
	     is easier) doesn't help, since we won't know the name itself.  */
#if defined(HAVE_TZHEAD) && defined(TZDEFAULT)
	  tzdir = RETAIN([NSString stringWithUTF8String: TZDIR]);
	  localZoneString = [NSString stringWithUTF8String: TZDEFAULT];
          localZoneSource = [NSString stringWithFormat:
	    @"file (TZDEFAULT): '%@'", localZoneString];
	  localZoneString = [localZoneString stringByResolvingSymlinksInPath];
#else
          if ([dflt fileExistsAtPath: SYSTEM_TIME_FILE])
	    {
	      localZoneString = SYSTEM_TIME_FILE;
	      localZoneSource = [NSString stringWithFormat:
		@"file (SYSTEM_TIME_FILE): '%@'", localZoneString];
	      localZoneString
		= [localZoneString stringByResolvingSymlinksInPath];
	      /* Guess what tzdir is */
	      tzdir = [localZoneString stringByDeletingLastPathComponent];
	      while ([tzdir length] > 2
		&& [dflt fileExistsAtPath:
		[tzdir stringByAppendingPathComponent: @"GMT"]] == NO)
		{
		  tzdir = [tzdir stringByDeletingLastPathComponent];
		}
	      if ([tzdir length] <= 2)
	        {
		  localZoneString = tzdir = nil;
		}
	      else
		{
		  [tzdir retain];
		}
	    }
#endif
	  if (localZoneString != nil && [localZoneString hasPrefix: tzdir])
	    {
	      /* This must be the time zone name */
	      localZoneString = AUTORELEASE([localZoneString mutableCopy]);
	      [(NSMutableString*)localZoneString deleteCharactersInRange:
		NSMakeRange(0, [tzdir length])];
	      while ([localZoneString hasPrefix: @"/"])
	        {
		  [(NSMutableString*)localZoneString deleteCharactersInRange:
		    NSMakeRange(0, 1)];
	        }
	    }
	  else
	    {
	      localZoneString = nil;
	    }
        }

      /* Try to get timezone from standard unix environment variable.
       * This is often an ambiguous abbreviation :-(
       */
      if (localZoneString == nil)
	{
          localZoneSource = _(@"environment variable: 'TZ'");
	  localZoneString = [[[NSProcessInfo processInfo]
	    environment] objectForKey: @"TZ"];
	}


#if HAVE_TZSET && !defined(__FreeBSD__) && !defined(__OpenBSD__)
      /*
       * Try to get timezone from tzset and tzname/daylight.
       * If daylight is non-zero, then tzname[0] is only the name
       * the the zone for part of the year, so we can't use it as
       * the definitive zone.
       *
       * FreeBSD doesn't implement TZSet fully, so we can't use it there.
       * Apparently, OpenBSD neither.
       */
      if (localZoneString == nil)
	{
          localZoneSource = @"function: 'tzset()/tzname'";
	  tzset();
	  if (NULL != tzname[0] && '\0' != *tzname[0] && 0 == daylight)
	    localZoneString = [NSString stringWithUTF8String: tzname[0]];
	}
#endif

      if (localZoneString != nil)
	{
	  NSDebugLLog (@"NSTimeZone", @"Using zone %@", localZoneString);
	  zone = [defaultPlaceholderTimeZone initWithName: localZoneString];
	  if (zone == nil)
	    {
	      NSArray	*possibleZoneNames;

	      /*
		It is not guaranteed on some systems (e.g., Ubuntu) that 
		SYSTEM_TIME_FILE is a symlink. This file is more probably 
		a copy of a zoneinfo file. The above time zone detecting 
		approach can lead to the situation when we can only know 
		about the time zone abbreviation (localZoneString) and 
		(for some time zone abbreviations) the corresponding list 
		of possible time zone names (e.g. SAMT is valid for
		Pacific/Samoa, Pacific/Pago_Pago, Pacific/Apia,
		Asia/Samarkand, Europe/Samara, US/Samoa).
		In such a case the time zone can be selected 
		from the list by comparing the content of SYSTEM_TIME_FILE 
		and the content of zoneinfo files corresponding to the items 
		of that list.
	       */
	      possibleZoneNames = [[self abbreviationMap]
		objectForKey: localZoneString];
	      if (possibleZoneNames != nil)
		{
		  NSEnumerator	*en = [possibleZoneNames objectEnumerator];
		  NSString	*zoneName;
		  NSFileManager *dflt = [NSFileManager defaultManager];
		  
		  while ((zoneName = [en nextObject]) != nil)
		    {
		      NSString	*fileName = [self _getTimeZoneFile: zoneName];

		      if (fileName != nil
			&& [dflt contentsEqualAtPath: fileName 
					     andPath: SYSTEM_TIME_FILE])
			{
			  zone = [[self timeZoneWithName: zoneName] retain];

			  if (zone != nil)
			    {
			      GSPrintf(stderr,
@"\nIt seems that your operating system does not have a valid timezone name\n"
@"configured and is using an abbreviation instead.  By comparing timezone\n"
@"file data it is has been possible to find the actual timezone used, but\n"
@"doing that is a slow process.\n"
@"\nYou can avoid slowness of this time zone detecting approach\n"
@"by setting the environment variable TZ='%@'\n"
@"Or You can override the timezone name by setting the '%@'\n"
@"NSUserDefault via the 'defaults' command line utility, a Preferences\n"
@"application, or some other utility.\n"
@"eg \"defaults write NSGlobalDomain '%@' '%@'\"\n\n",
zoneName, LOCALDBKEY, LOCALDBKEY, zoneName);
			      break;
			    }
			}
		    }
		}
	    }
	  if (zone == nil)
	    {
	      if (zone == nil)
		{
		  GSPrintf(stderr,
@"\nUnable to create time zone for name: '%@'\n"
@"(source '%@').\n", localZoneString, localZoneSource);
		}
	      if ([localZoneSource hasPrefix: @"file"]
	        || [localZoneSource hasPrefix: @"function"])
		{
                  GSPrintf(stderr,
@"\nIt seems that your operating system does not have a valid timezone name\n"
@"configured (it could be that some other software has set a, possibly\n"
@"ambiguous, timezone abbreviation rather than a name) ... please correct\n"
@"that or override by setting a timezone name (such as 'Europe/London'\n"
@"or 'America/Chicago').\n");
		}
	      GSPrintf(stderr,
@"\nYou can override the timezone name by setting the '%@'\n"
@"NSUserDefault via the 'defaults' command line utility, a Preferences\n"
@"application, or some other utility.\n"
@"eg \"defaults write NSGlobalDomain '%@' 'Africa/Nairobi'\"\n"
@"See '%@'\n"
@"for the standard timezones such as 'GB-Eire' or 'America/Chicago'.\n",
LOCALDBKEY, LOCALDBKEY, _time_zone_path (ZONES_DIR, nil));
	      zone = [[self timeZoneWithAbbreviation: localZoneString] retain];
	      if (zone != nil)
		{
		  NSInteger	s;
		  char		sign = '+';

		  s = [zone secondsFromGMT];
		  if (s < 0)
		    {
		      sign = '-';
		      s = -s;
		    }
	          GSPrintf(stderr,
@"\nSucceeded in treating '%@' as a timezone abbreviation,\n"
@"but abbreviations do not uniquely represent timezones, so this may\n"
@"not have found the timezone you were expecting.  The timezone found\n"
@"was '%@' (currently UTC%c%02d%02d)\n\n",
localZoneString, [zone name], sign, s/3600, (s/60)%60);
		}
	    }
	}
      else
	{
	  NSLog(@"No local time zone specified.");
	}

      /*
       * If local time zone fails to allocate, then allocate something
       * that is sure to succeed (unless we run out of memory, of
       * course).
       */
      if (zone == nil)
        {
          NSLog(@"Using time zone with absolute offset 0.");
          zone = systemTimeZone;
        }
      ASSIGN(systemTimeZone, zone);
    }
  zone = AUTORELEASE(RETAIN(systemTimeZone));
  pthread_mutex_unlock(&zone_mutex);
  return zone;
}

/**
 * Returns an array of all the known regions.<br />
 * There are 24 elements, of course, one for each time zone.
 * Each element contains an array of NSStrings which are
 * the region names.
 */
+ (NSArray*) timeZoneArray
{
  static NSArray *regionsArray = nil;

  /* We create the array only when we need it to reduce overhead. */
  if (regionsArray != nil)
    {
      return regionsArray;
    }
  pthread_mutex_lock(&zone_mutex);
  if (regionsArray == nil)
    {
      NSAutoreleasePool	*pool = [NSAutoreleasePool new];
      int		index;
      int		i;
      char		name[80];
      FILE		*fp;
      NSMutableArray	*temp_array[24];
      NSString		*path;

      for (i = 0; i < 24; i++)
	{
	  temp_array[i] = [NSMutableArray array];
	}

      path = _time_zone_path (REGIONS_FILE, nil);
      if (path != nil)
	{
#if	defined(_WIN32)
	  unichar	mode[3];

	  mode[0] = 'r';
	  mode[1] = 'b';
	  mode[2] = '\0';

	  fp = _wfopen((const unichar*)[path fileSystemRepresentation], mode);
#else
	  fp = fopen([path fileSystemRepresentation], "r");
#endif
	  if (fp == NULL)
	    {
              pthread_mutex_unlock(&zone_mutex);
	      [NSException
		raise: NSInternalInconsistencyException
		format: @"Failed to open time zone regions array file."];
	    }
	  while (fscanf(fp, "%d %79s", &index, name) == 2)
	    {
              if (index < 0)
                index = 0;
              else
                index %= 24;
	      [temp_array[index]
		addObject: [NSString stringWithUTF8String: name]];
	    }
	  fclose(fp);
	}
      else
	{
	  NSString	*zonedir = [NSTimeZone _getTimeZoneFile: @"WET"]; 

	  if (tzdir != nil)
	    {
	      NSFileManager		*mgr = [NSFileManager defaultManager];
	      NSDirectoryEnumerator	*enumerator;
	      NSString			*name;

	      zonedir = [zonedir stringByDeletingLastPathComponent];
	      enumerator = [mgr enumeratorAtPath: zonedir];
	      while ((name = [enumerator nextObject]) != nil)
		{
		  NSTimeZone	*zone = nil;
		  BOOL		isDir;
		
		  path = [zonedir stringByAppendingPathComponent: name];
		  if ([mgr fileExistsAtPath: path isDirectory: &isDir]
                    && isDir == NO
                    && [[path pathExtension] isEqual: @"tab"] == NO)
		    {
		      zone = [zoneDictionary objectForKey: name];
		      if (zone == nil)
			{
			  NSData	*data;

			  data = [NSData dataWithContentsOfFile: path];
			  /* We should really make sure this is a real
			     zone file and not something extra that happens
			     to be in this directory, but initWithName:data:
			     will do this anyway and log a message if not. */
			  zone = [[self alloc] initWithName: name data: data];
			  IF_NO_GC([zone autorelease];)
			}
		      if (zone != nil)
			{
			  int			offset;
			  NSArray		*details;
			  NSTimeZoneDetail	*detail;
			  NSEnumerator		*e;

			  details = [zone timeZoneDetailArray];
			  e = [details objectEnumerator];
		
			  while ((detail = [e nextObject]) != nil)
			    {
			      if ([detail isDaylightSavingTime] == NO)
				{
				  break;	// Found a standard time
				}
			    }
			  if (detail == nil && [details count] > 0)
			    {
			      // If no standard time
			      detail = [details objectAtIndex: 0];
			    }

			  offset = [detail secondsFromGMT];
			  if (offset < 0)
			    {
			      offset = -offset;
			      offset %= (60 * 60 * 24);
                              if (offset > 0)
                                {
                                  offset = -offset;
                                  offset += (60 * 60 * 24);
                                }
			    }
			  else
			    {
			      offset %= (60 * 60 * 24);
			    }
			  offset /= (60 * 60);

			  [temp_array[offset] addObject: name];
			}
		    }
		}
	    }
	}
      regionsArray = [[NSArray alloc] initWithObjects: temp_array count: 24];
      [pool drain];
    }
  pthread_mutex_unlock(&zone_mutex);
  return regionsArray;
}

/**
 * Return a timezone for the specified offset from GMT.<br />
 * The timezone returned does <em>not</em> use daylight savings time.
 * The actual timezone returned has an offset rounded to the nearest
 * minute.<br />
 * Time zones with an offset of more than +/- 18 hours  are disallowed,
 * and nil is returned.
 */
+ (NSTimeZone*) timeZoneForSecondsFromGMT: (NSInteger)seconds
{
  NSTimeZone	*zone;
  int		sign = seconds >= 0 ? 1 : -1;
  int           extra;

  /*
   * Round the offset to the nearest minute, (for MacOS-X compatibility)
   * and ensure it is no more than 18 hours.
   */
  seconds *= sign;
  extra = seconds % 60;
  if (extra < 30)
    {
      seconds -= extra;
    }
  else
    {
      seconds += 60 - extra;
    }
  if (seconds > 64800)
    {
      return nil;
    }
  seconds *= sign;
  if (seconds % 900 == 0)
    {
      zone = commonAbsolutes[seconds/900 + 72];
    }
  else
    {
      pthread_mutex_lock(&zone_mutex);
      zone = (NSTimeZone*)NSMapGet(absolutes, (void*)(uintptr_t)seconds);
      pthread_mutex_unlock(&zone_mutex);
    }
  if (nil == zone)
    {
      zone = [[GSAbsTimeZone alloc] initWithOffset: seconds name: nil];
      zone = AUTORELEASE(zone);
    }
  return zone;
}

/**
 * Returns a timezone for the specified abbreviation. The same abbreviations
 * are used in different regions so this isn't particularly useful.<br />
 * Calls NSTimeZone-abbreviation dictionary an so uses a lot of memory.
 */
+ (NSTimeZone*) timeZoneWithAbbreviation: (NSString*)abbreviation
{
  NSTimeZone	*zone;
  NSString	*name;

  name = [[self abbreviationDictionary] objectForKey: abbreviation];
  if (name == nil)
    {
      zone = nil;
    }
  else
    {
      zone = [self timeZoneWithName: name data: nil];
    }
  return zone;
}

/**
 * Returns a timezone for the specified name.
 */
+ (NSTimeZone*) timeZoneWithName: (NSString*)aTimeZoneName
{
  NSTimeZone	*zone;

  zone = [defaultPlaceholderTimeZone initWithName: aTimeZoneName data: nil];
  return AUTORELEASE(zone);
}

/**
 * Returns a timezone for aTimeZoneName, created from the supplied
 * time zone data. Data must be in TZ format as per the Olson database.
 */
+ (NSTimeZone*) timeZoneWithName: (NSString*)name data: (NSData*)data
{
  NSTimeZone	*zone;

  zone = [defaultPlaceholderTimeZone initWithName: name data: data];
  return AUTORELEASE(zone);
}

/**
 * Returns the abbreviation for this timezone now.
 * Invokes -abbreviationForDate:
 */
- (NSString*) abbreviation
{
  return [self abbreviationForDate: [NSDate date]];
}

/**
 * Returns the abbreviation for this timezone at aDate.  This may differ
 * depending on whether daylight savings time is in effect or not.
 */
- (NSString*) abbreviationForDate: (NSDate*)aDate
{
  NSTimeZoneDetail	*detail;
  NSString		*abbr;

  detail = [self timeZoneDetailForDate: aDate];
  abbr = [detail timeZoneAbbreviation];

  return abbr;
}

/**
 * Returns the Class for this object
 */
- (Class) classForCoder
{
  return NSTimeZoneClass;
}

- (id) copyWithZone: (NSZone*)z
{
  return RETAIN(self);
}

/**
 * Returns the data with which the receiver was initialised.
 */
- (NSData*) data
{
  return nil;
}

/**
 * Returns the name of this object.
 */
- (NSString*) description
{
  return [self name];
}

- (void) encodeWithCoder: (NSCoder*)aCoder
{
  [aCoder encodeObject: [self name]];
}

- (NSUInteger) hash
{
  return [[self name] hash];
}

- (id) init
{
  return [self initWithName: @"NSLocalTimeZone" data: nil];
}

- (id) initWithCoder: (NSCoder*)aDecoder
{
  NSString	*name;

  name = [aDecoder decodeObject];
  self = [self initWithName: name data: nil];
  return self;
}

/**
 * Initialise a timezone with the supplied name.  May return a cached
 * timezone object rather than the newly created one.
 */
- (id) initWithName: (NSString*)name
{
  return [self initWithName: name data: nil];
}

/**
 * Initialises a time zone object using the supplied data object.<br />
 * This method is intended for internal use by the NSTimeZone
 * class cluster.
 * Don't use it ... use -initWithName: instead.
 */
- (id) initWithName: (NSString*)name data: (NSData*)data
{
  [self notImplemented: _cmd];
  return nil;
}

/**
 * Returns a boolean indicating whether daylight savings time is in
 * effect now.  Invokes -isDaylightSavingTimeForDate:
 */
- (BOOL) isDaylightSavingTime
{
  return [self isDaylightSavingTimeForDate: [NSDate date]];
}

/**
 * Returns a boolean indicating whether daylight savings time is in
 * effect for this time zone at aDate.
 */
- (BOOL) isDaylightSavingTimeForDate: (NSDate*)aDate
{
  NSTimeZoneDetail	*detail;
  BOOL			isDST;

  detail = [self timeZoneDetailForDate: aDate];
  isDST = [detail isDaylightSavingTimeZone];

  return isDST;
}

- (BOOL) isEqual: (id)other
{
  if (other == self)
    return YES;
  if ([other isKindOfClass: NSTimeZoneClass] == NO)
    return NO;
  return [self isEqualToTimeZone: other];
}

/**
 * Returns YES if the time zones have the same name.
 */
- (BOOL) isEqualToTimeZone: (NSTimeZone*)aTimeZone
{
  if (aTimeZone == self)
    return YES;
  if ([[self name] isEqual: [aTimeZone name]] == NO)
    return NO;
  if (([self data] == nil && [aTimeZone data] == nil)
    || [[self name] isEqual: [aTimeZone name]] == YES)
    return YES;
  return NO;
}

/**
 * Returns the name of the timezone
 */
- (NSString*) name
{
  return [self subclassResponsibility: _cmd];
}

- (id) replacementObjectForPortCoder: (NSPortCoder*)aCoder
{
  if ([aCoder isByref] == NO)
    {
      return self;
    }
  return [super replacementObjectForPortCoder: aCoder];
}

/**
 * Returns the number of seconds by which the receiver differs
 * from Greenwich Mean Time at the current date and time.<br />
 * Invokes -secondsFromGMTForDate:
 */
- (NSInteger) secondsFromGMT
{
  return [self secondsFromGMTForDate: [NSDate date]];
}

/**
 * Returns the number of seconds by which the receiver differs
 * from Greenwich Mean Time at the date aDate.<br />
 * If the time zone uses daylight savings time, the returned value
 * will vary at different times of year.
 */
- (NSInteger) secondsFromGMTForDate: (NSDate*)aDate
{
  NSTimeZoneDetail	*detail;
  int			offset;

  detail = [self timeZoneDetailForDate: aDate];
  offset = [detail timeZoneSecondsFromGMT];

  return offset;
}

/**
 * DEPRECATED:  see NSTimeZoneDetail
 */
- (NSArray*) timeZoneDetailArray
{
  return [self subclassResponsibility: _cmd];
}

/**
 * DEPRECATED:  see NSTimeZoneDetail
 */
- (NSTimeZoneDetail*) timeZoneDetailForDate: (NSDate*)date
{
  return [self subclassResponsibility: _cmd];
}

/**
 * Returns the name of this timezone.
 */
- (NSString*) timeZoneName
{
  return [self name];
}

- (NSTimeInterval) daylightSavingTimeOffsetForDate: (NSDate *)aDate
{
#if GS_USE_ICU == 1
  NSTimeInterval result;
  UCalendar *cal;
  UErrorCode err = U_ZERO_ERROR;
  
  cal = ICUCalendarSetup (self, nil);
  if (cal == NULL)
    return 0.0;
  
  ucal_setMillis (cal, ([aDate timeIntervalSince1970] * 1000.0), &err);
  result = (double)ucal_get (cal, UCAL_DST_OFFSET, &err) / 1000.0;
  if (U_FAILURE(err))
    result = 0.0;
  ucal_close (cal);
  
  return result;
#else
  return 0.0;   // FIXME
#endif
}

- (NSDate *) nextDaylightSavingTimeTransitionAfterDate: (NSDate *)aDate
{
#if GS_USE_ICU == 1
  /* ICU doesn't provide transition information per se.
   * The canonical method of retrieving this piece of information is to
   * use binary search.
   */
  
  int32_t originalOffset, currentOffset;
  UCalendar *cal;
  UErrorCode err = U_ZERO_ERROR;
  UDate currentTime;
  int i;
  NSDate* result = nil;
  
  cal = ICUCalendarSetup (self, nil);
  if (cal == NULL)
    return nil;
  
  currentTime = [aDate timeIntervalSince1970] * 1000.0;
  ucal_setMillis (cal, currentTime, &err);
  originalOffset = ucal_get (cal, UCAL_DST_OFFSET, &err);
  if (U_FAILURE(err))
    return nil;

  /* First try to find the next transition by adding a week at a time */
  /* Avoid ending in an infinite loop in case there is no transition at all */

  for (i = 0; i < 53; i++)
    {
      /* Add a single week */
      currentTime += WEEK_MILLISECONDS;

	  ucal_setMillis (cal, currentTime, &err);
	  if (U_FAILURE(err))
        break;

	  currentOffset = ucal_get (cal, UCAL_DST_OFFSET, &err);
	  if (U_FAILURE(err))
        break;

	  if (currentOffset != originalOffset)
        {
          double interval = WEEK_MILLISECONDS / 2.0;
          /* Now use bisection to determine the exact moment */

		  while (interval >= 1.0)
            {
              ucal_setMillis (cal, currentTime - interval, &err);

              currentOffset = ucal_get (cal, UCAL_DST_OFFSET, &err);

              if (currentOffset != originalOffset)
                currentTime -= interval; /* it is in the lower half */

              interval /= 2.0;
            }

           result =
             [NSDate dateWithTimeIntervalSince1970: floor(currentTime/1000.0)];
        }
    }

  ucal_close (cal);
  
  return result;
#else
  return nil;   // FIXME;
#endif
}

- (NSTimeInterval) daylightSavingTimeOffset
{
  return [self daylightSavingTimeOffsetForDate: [NSDate date]];
}

- (NSDate *) nextDaylightSavingTimeTransition
{
  return [self nextDaylightSavingTimeTransitionAfterDate: [NSDate date]];
}

- (NSString *)localizedName: (NSTimeZoneNameStyle)style
                     locale: (NSLocale *)locale
{
#if GS_USE_ICU == 1
  UChar *result;
  const char *cLocale;
  int32_t len;
  UCalendar *cal;
  UErrorCode err = U_ZERO_ERROR;
  
  cal = ICUCalendarSetup (self, locale);
  if (cal == NULL)
    return nil;
  
  cLocale = [[locale localeIdentifier] UTF8String];
  result = NSZoneMalloc ([self zone], BUFFER_SIZE * sizeof(UChar));
  len = ucal_getTimeZoneDisplayName (cal, _NSToICUTZDisplayStyle(style),
    cLocale, result, BUFFER_SIZE, &err);
  if (len > BUFFER_SIZE)
    {
      result = NSZoneRealloc ([self zone], result, len * sizeof(UChar));
      ucal_getTimeZoneDisplayName (cal, _NSToICUTZDisplayStyle(style),
        cLocale, result, len, &err);
    }
  
  return AUTORELEASE([[NSString alloc] initWithCharactersNoCopy: result
    length: len freeWhenDone: YES]);
#else
  return nil;   // FIXME;
#endif
}
                    
@end

/**
 * This class serves no useful purpose in GNUstep other than to provide
 * a backup mechanism for handling abbreviations where the precomputed
 * data files cannot be found. It is provided primarily for backward
 * compatibility with the OpenStep spec.  It is missing entirely from MacOS-X.
 */
@implementation NSTimeZoneDetail

- (NSString*) description
{
  return [NSString stringWithFormat: @"%@(%@, %s%"PRIdPTR")", [self name],
    [self timeZoneAbbreviation],
    ([self isDaylightSavingTimeZone]? "IS_DST, ": ""),
    [self timeZoneSecondsFromGMT]];
}

/**
 * DEPRECATED: Class is no longer used.
 */
- (BOOL) isDaylightSavingTimeZone
{
  [self subclassResponsibility: _cmd];
  return NO;
}

/**
 * DEPRECATED: Class is no longer used.
 */
- (NSString*) timeZoneAbbreviation
{
  return [self subclassResponsibility: _cmd];
}

/**
 * DEPRECATED: Class is no longer used.
 */
- (NSInteger) timeZoneSecondsFromGMT
{
  [self subclassResponsibility: _cmd];
  return 0;
}

@end


@implementation NSTimeZone (Private)

/**
 * Common locations for timezone info on unix systems.
 */
static NSString *zoneDirs[] = {
#ifdef TZDIR
  @TZDIR,
#endif
  @"/usr/share/zoneinfo", 
  @"/usr/lib/zoneinfo",
  @"/usr/local/share/zoneinfo",
  @"/usr/local/lib/zoneinfo", 
  @"/etc/zoneinfo",
  @"/usr/local/etc/zoneinfo"
};

/**
 * Returns the path to the named zone info file.
 */
+ (NSString*) _getTimeZoneFile: (NSString *)name
{
  static BOOL	beenHere = NO;
  NSString	*dir = nil;
  BOOL		isDir;

  if (beenHere == NO && tzdir == nil)
    {
      pthread_mutex_lock(&zone_mutex);
      if (beenHere == NO && tzdir == nil)
	{
	  NSFileManager	*mgr = [NSFileManager defaultManager];
	  NSString	*zonedir = nil;
	  unsigned	i;

	  for (i = 0; i < sizeof(zoneDirs)/sizeof(zoneDirs[0]); i++)
	    {
	      BOOL	isDir;

	      zonedir
		= [zoneDirs[i] stringByAppendingPathComponent: POSIX_TZONES];
	      if ([mgr fileExistsAtPath: zonedir isDirectory: &isDir] && isDir)
		{
		  tzdir = RETAIN(zonedir);
		  break;  // use first one
		}
	    }
	  beenHere = YES;
	}
      pthread_mutex_unlock(&zone_mutex);
    }
  /* Use the system zone info if possible, otherwise, use our installed
     info.  */
  if (tzdir && [[NSFileManager defaultManager] fileExistsAtPath:
    [tzdir stringByAppendingPathComponent: name] isDirectory: &isDir] == YES
    && isDir == NO)
    {
      dir = tzdir;
    }
  if (dir == nil)
    {
      dir = _time_zone_path (ZONES_DIR, nil);
    }
  return [dir stringByAppendingPathComponent: name];
}

+ (void) _notified: (NSNotification*)n
{
  NSString      *name;

  /* If the name of the system time zone has changed ...
   * get a new system time zone.
   */
  name = [[NSUserDefaults standardUserDefaults] stringForKey: LOCALDBKEY];
  if ([name length] > 0 && [name isEqual: [[self systemTimeZone] name]] == NO)
    {
      [self resetSystemTimeZone];
      [self systemTimeZone];
    }
}
@end


#if	defined(_WIN32)
/* Timezone information data as stored in the registry */
typedef struct TZI_format {
	LONG       Bias;
	LONG       StandardBias;
	LONG       DaylightBias;
	SYSTEMTIME StandardDate;
	SYSTEMTIME DaylightDate;
} TZI;

static inline unsigned int
lastDayOfGregorianMonth(int month, int year)
{
  switch (month)
    {
      case 2:
        if ((((year % 4) == 0) && ((year % 100) != 0))
          || ((year % 400) == 0))
          return 29;
        else
          return 28;
      case 4:
      case 6:
      case 9:
      case 11: return 30;
      default: return 31;
    }
}

/* IMPORT from NSCalendar date */
void
GSBreakTime(NSTimeInterval when,
  NSInteger *year, NSInteger *month, NSInteger *day,
  NSInteger *hour, NSInteger *minute, NSInteger *second, NSInteger *mil);


@implementation GSWindowsTimeZone

- (NSString*) abbreviationForDate: (NSDate*)aDate
{
  if ([self isDaylightSavingTimeForDate: aDate])
    return daylightZoneNameAbbr;
  return timeZoneNameAbbr;
}

- (NSData*) data
{
  return 0;
}

- (void) dealloc
{
  RELEASE(timeZoneName);
  RELEASE(daylightZoneName);
  RELEASE(timeZoneNameAbbr);
  RELEASE(daylightZoneNameAbbr);
  [super dealloc];
}

- (id) initWithName: (NSString*)name data: (NSData*)data
{
  HKEY	regDirKey;
  BOOL	isNT = NO;
  BOOL	regFound=NO;
  BOOL	tzFound = NO;

  /* Open the key in the local machine hive where
   * the time zone data is stored. */
  if (ERROR_SUCCESS == RegOpenKeyExW(HKEY_LOCAL_MACHINE,
    L"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones",
    0,
    KEY_READ,
    &regDirKey))
    {
      isNT = YES;
      regFound = YES;
    }
  else
    {
      if (ERROR_SUCCESS == RegOpenKeyExW(HKEY_LOCAL_MACHINE,
          L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Time Zones",
          0,
          KEY_READ,
          &regDirKey))
        {
          regFound = YES;
        }
    }

  if (regFound)
    {
      /* Iterate over all subKeys in the registry to find the right one.
         Unfortunately name is a localized value. The keys in the registry are
         unlocalized names. */
      wchar_t  achKey[255];              // buffer for subkey name
      DWORD    cbName;                   // size of name string 
      wchar_t  achClass[MAX_PATH] = L""; // buffer for class name 
      DWORD    cchClassName = MAX_PATH;  // size of class string 
      DWORD    cSubKeys = 0;             // number of subkeys 
      DWORD    cbMaxSubKey;              // longest subkey size 
      DWORD    cchMaxClass;              // longest class string 
      DWORD    cValues;                  // number of values for key 
      DWORD    cchMaxValue;              // longest value name 
      DWORD    cbMaxValueData;           // longest value data 
      DWORD    cbSecurityDescriptor;     // size of security descriptor 
      FILETIME ftLastWriteTime;          // last write time 
      DWORD     i, retCode;
		
      /* Get the class name and the value count. */
      retCode = RegQueryInfoKeyW(
        regDirKey,               // key handle 
        achClass,                // buffer for class name 
        &cchClassName,           // size of class string 
        NULL,                    // reserved 
        &cSubKeys,               // number of subkeys 
        &cbMaxSubKey,            // longest subkey size 
        &cchMaxClass,            // longest class string 
        &cValues,                // number of values for this key 
        &cchMaxValue,            // longest value name 
        &cbMaxValueData,         // longest value data 
        &cbSecurityDescriptor,   // security descriptor 
        &ftLastWriteTime);       // last write time 

      if (cSubKeys && (retCode == ERROR_SUCCESS))
    	{
          unsigned wLen = [name length];
          wchar_t *wName = malloc((wLen+1) * sizeof(wchar_t));

          if (wName)
            {
              [name getCharacters: wName];
              wName[wLen] = 0;
              for (i = 0; i < cSubKeys && !tzFound; i++) 
                { 
                  cbName = 255;
                   
                  retCode = RegEnumKeyExW(regDirKey, i, achKey, &cbName,
                    NULL, NULL, NULL, &ftLastWriteTime);         
                  if (retCode == ERROR_SUCCESS) 
                    {
                      wchar_t keyBuffer[16384];
                      HKEY regKey;
                       
                      if (isNT)
                        wcscpy(keyBuffer, L"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\");
                      else
                        wcscpy(keyBuffer, L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Time Zones\\");
                       
                      wcscat(keyBuffer, achKey);
                      if (ERROR_SUCCESS == RegOpenKeyExW(HKEY_LOCAL_MACHINE,
                        keyBuffer, 0, KEY_READ, &regKey))
                        {
                          wchar_t buf[256];
                          wchar_t standardName[256];
                          wchar_t daylightName[256];
                          DWORD bufsize;
                          DWORD type;

                          /* check standardname */
                          standardName[0] = L'\0';
                          bufsize = sizeof(buf);
                          if (ERROR_SUCCESS == RegQueryValueExW(regKey,
                            L"Std", 0, &type, (BYTE *)buf, &bufsize))
                            {
                              wcscpy(standardName, buf);
                              if (wcscmp(standardName, wName) == 0)
                                tzFound = YES;
                            }
        
                          /* check daylightname */
                          daylightName[0] = L'\0';
                          bufsize = sizeof(buf);
                          if (ERROR_SUCCESS == RegQueryValueExW(regKey,
                            L"Dlt", 0, &type, (BYTE *)buf, &bufsize))
                            {
                              wcscpy(daylightName, buf);
                              if (wcscmp(daylightName, wName) == 0)
                                tzFound = YES;
                            }

                          if (tzFound)
                            {
                              /* Read in the time zone data */
                              bufsize = sizeof(buf);
                              if (ERROR_SUCCESS == RegQueryValueExW(regKey,
                                L"TZI", 0, &type, (BYTE *)buf, &bufsize))
                                {
                                  TZI *tzi = (void*)buf;
                                  Bias = tzi->Bias;
                                  StandardBias = tzi->StandardBias;
                                  DaylightBias = tzi->DaylightBias;
                                  StandardDate = tzi->StandardDate;
                                  DaylightDate = tzi->DaylightDate;
                                }
                          
                              /* Set the standard name for the time zone. */
                              if (wcslen(standardName))
                                {
                                  int a, b;

                                  ASSIGN(timeZoneName,
                                    [NSString stringWithCharacters: standardName
                                    length: wcslen(standardName)]);

                                  /* Abbr generated here is IMHO
                                   * a bit suspicous but I kept it */
                                  for (a = 0, b = 0; standardName[a]; a++)
                                    {
                                      if (iswupper(standardName[a]))
                                        standardName[b++] = standardName[a];
                                    }
                                  standardName[b] = L'\0';
                                  ASSIGN(timeZoneNameAbbr,
                                    [NSString stringWithCharacters: standardName
                                    length: wcslen(standardName)]);
                                }

                              /* Set the daylight savings name
                               * for the time zone. */
                              if (wcslen(daylightName))
                                {
                                  int a, b;

                                  ASSIGN(daylightZoneName,
                                    [NSString stringWithCharacters: daylightName
                                    length: wcslen(daylightName)]);

                                  /* Abbr generated here is IMHO
                                   * a bit suspicous but I kept it */
                                  for (a = 0, b = 0; daylightName[a]; a++)
                                    {
                                      if (iswupper(daylightName[a]))
                                        daylightName[b++] = daylightName[a];
                                    }
                                  daylightName[b] = L'\0';
                                  ASSIGN(daylightZoneNameAbbr,
                                    [NSString stringWithCharacters: daylightName
                                    length: wcslen(daylightName)]);
                                }
                            }
                          RegCloseKey(regKey);
                        }			       
                    }
                }
              free(wName);
            }
        }
      RegCloseKey(regDirKey);
    }
  if (NO == tzFound)
    {
      DESTROY(self);
    }
  return self;
}

- (BOOL) isDaylightSavingTimeForDate: (NSDate*)aDate
{
  NSInteger year, month, day, hour, minute, second, mil;
  int	dow;
  int daylightdate, count, maxdate;
  NSTimeInterval when;

  if (DaylightDate.wMonth == 0)
    return NO;

  when = [aDate timeIntervalSinceReferenceDate] - Bias*60;

  GSBreakTime(when, &year, &month, &day, &hour, &minute, &second, &mil);

  // Check north globe
  if (StandardDate.wMonth >= DaylightDate.wMonth)
    {
      // Before April or after October is Std
      if (month < DaylightDate.wMonth || month > StandardDate.wMonth)
        {
	  return NO;
	}
      // After April and before October is DST
      if (month > DaylightDate.wMonth && month < StandardDate.wMonth)
        {
	  return YES;
	}
    }
  else
    {
      /* check south globe 
       * Before April or after October is DST
       */
      if (month < StandardDate.wMonth || month > DaylightDate.wMonth)
        {
	  return YES;
	}
      // After April and before October is Std
      if (month > StandardDate.wMonth && month < DaylightDate.wMonth)
        {
	  return NO;
	}
    }

  dow = ((NSInteger)((when / 86400.0) + GREGORIAN_REFERENCE)) % 7;
  if (dow < 0)
    dow += 7;

  if (month == DaylightDate.wMonth /* April */)
    {
      daylightdate = day - dow + DaylightDate.wDayOfWeek;
      maxdate = lastDayOfGregorianMonth(DaylightDate.wMonth, year)-7;
      while (daylightdate > 7)
        daylightdate -= 7;
      if (daylightdate < 1)
        daylightdate += 7;
      count = DaylightDate.wDay;
      while (count > 1 && daylightdate < maxdate)
        {
          daylightdate += 7;
          count--;
        }
      if (day > daylightdate)
        return YES;
      if (day < daylightdate)
        return NO;
      if (hour > DaylightDate.wHour)
        return YES;
      if (hour < DaylightDate.wHour)
        return NO;
      if (minute > DaylightDate.wMinute)
        return YES;
      if (minute < DaylightDate.wMinute)
        return NO;
      if (second > DaylightDate.wSecond)
        return YES;
      if (second < DaylightDate.wSecond)
        return NO;
      if (mil >= DaylightDate.wMilliseconds)
        return YES;
      return NO;
    }
  if (month == StandardDate.wMonth /* October */)
    {
      daylightdate = day - dow + StandardDate.wDayOfWeek;
      maxdate = lastDayOfGregorianMonth(StandardDate.wMonth, year)-7;
      while (daylightdate > 7)
        daylightdate -= 7;
      if (daylightdate < 1)
        daylightdate += 7;
      count = StandardDate.wDay;
      while (count > 1 && daylightdate < maxdate)
        {
          daylightdate += 7;
          count--;
        }
      if (day > daylightdate)
        return NO;
      if (day < daylightdate)
        return YES;
      if (hour > StandardDate.wHour)
        return NO;
      if (hour < StandardDate.wHour)
        return YES;
      if (minute > StandardDate.wMinute)
        return NO;
      if (minute < StandardDate.wMinute)
        return YES;
      if (second > StandardDate.wSecond)
        return NO;
      if (second < StandardDate.wSecond)
        return YES;
      if (mil >= StandardDate.wMilliseconds)
        return NO;
      return YES;
    }
  return NO; // Never reached
}

- (NSString*) name
{
  TIME_ZONE_INFORMATION tz;
  DWORD DST = GetTimeZoneInformation(&tz);

  if (DST == TIME_ZONE_ID_DAYLIGHT)
    {
      return daylightZoneName;
    }
  else
    {
      return timeZoneName;
    }
}

- (NSInteger) secondsFromGMTForDate: (NSDate*)aDate
{
  if ([self isDaylightSavingTimeForDate: aDate])
    return -Bias*60 - DaylightBias*60;
  return -Bias*60 - StandardBias*60;
}

- (NSArray*) timeZoneDetailArray
{
  return [NSArray arrayWithObjects:
    [[[GSTimeZoneDetail alloc] initWithTimeZone: self
      withAbbrev: timeZoneNameAbbr
      withOffset: -Bias*60 - StandardBias*60
      withDST: NO] autorelease],
    [[[GSTimeZoneDetail alloc] initWithTimeZone: self
      withAbbrev: daylightZoneNameAbbr
      withOffset: -Bias*60 - DaylightBias*60
      withDST: YES] autorelease], 0];
}

- (NSTimeZoneDetail*) timeZoneDetailForDate: (NSDate*)aDate
{
  GSTimeZoneDetail	*detail;
  int offset;
  BOOL isDST = [self isDaylightSavingTimeForDate: aDate];
  NSString *abbr;

  if (isDST)
    {
      offset = -Bias*60 - DaylightBias*60;
      abbr = daylightZoneNameAbbr;
    }
  else
    {
      offset = -Bias*60 - StandardBias*60;
      abbr = timeZoneNameAbbr;
    }
  detail = [GSTimeZoneDetail alloc];
  detail = [detail initWithTimeZone: self
                         withAbbrev: abbr
                         withOffset: offset
                            withDST: isDST];
  return detail;
}

- (NSString*) timeZoneName
{
  return [self name];
}
@end
#endif // _WIN32


@implementation	GSTimeZone

static NSTimeZoneDetail*
newDetailInZoneForType(GSTimeZone *zone, TypeInfo *type)
{
  GSTimeZoneDetail	*detail;

  detail = [GSTimeZoneDetail alloc];
  detail = [detail initWithTimeZone: zone
			 withAbbrev: type->abbreviation
			 withOffset: type->offset
			    withDST: type->isdst];
  return detail;
}

/**
 * Obtain TypeInfo from transisions or TZstring
 */
static TypeInfo *
getTypeInfo(NSTimeInterval since, GSTimeZone *zone)
{
  int64_t		when = (int64_t)since;
  struct tm		tm;
  TypeInfo		*type;

  if (localsub(&zone->sp, &when, 0, &tm) == NULL)
    return NULL;

  type = NSZoneMalloc(NSDefaultMallocZone(), sizeof *type);
  type->offset = tm.tm_gmtoff;
  type->isdst = tm.tm_isdst;
  type->abbr_idx = 0;
  type->abbreviation = [NSString stringWithUTF8String:tm.tm_zone];

  return type;
}

- (NSString*) abbreviationForDate: (NSDate*)aDate
{
  TypeInfo	*type = getTypeInfo([aDate timeIntervalSince1970], self);

  return type->abbreviation;
}

- (NSData*) data
{
  return timeZoneData;
}

- (void) dealloc
{
  RELEASE(timeZoneName);
  RELEASE(timeZoneData);
  RELEASE(abbreviations);
  if (types != 0)
    {
      NSZoneFree(NSDefaultMallocZone(), types);
    }
  [super dealloc];
}

- (id) initWithName: (NSString*)name data: (NSData*)data
{
  static NSString	*fileException = @"GSTimeZoneFileException";

  timeZoneName = [name copy];
  timeZoneData = [data copy];
  NS_DURING
    {
      const void	*bytes = [timeZoneData bytes];
      unsigned		length = [timeZoneData length];
      void		*buf;
      unsigned		pos = 0;
      unsigned		i, charcnt;
      unsigned char	*abbr;
      struct tzhead	*header;
      unsigned		version = 1;

      if (length < sizeof(struct tzhead))
	{
	  [NSException raise: fileException
		      format: @"File is too small"];
	}
      header = (struct tzhead *)(bytes + pos);
      pos += sizeof(struct tzhead);

      if (memcmp(header->tzh_magic, TZ_MAGIC, strlen(TZ_MAGIC)) != 0)
	{
	  [NSException raise: fileException
		      format: @"TZ_MAGIC is incorrect"];
	}

      /* For version 2+, skip to second header */
      if (header->tzh_version[0] != 0)
        {
	  /* tzh_version[0] is an ascii digit for versions above 1.
	   */
	  version = header->tzh_version[0] - '0';

	  /* pos indexes after the first header, increment it to index after
	   * the data associated with that header too.
	   */
          pos += GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_timecnt) * 5
	    + GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_typecnt) * 6
	    + GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_charcnt)
	    + GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_leapcnt) * 8
	    + GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_ttisstdcnt)
	    + GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_ttisutcnt);

	  /* Now we get a pointer to the version 2+ header and set pos as an
	   * index to the data after that.
	   */
	  header = (struct tzhead *)(bytes + pos);
          pos += sizeof(struct tzhead);

          if (memcmp(header->tzh_magic, TZ_MAGIC, strlen(TZ_MAGIC)) != 0)
	    {
	      [NSException raise: fileException
		      format: @"TZ_MAGIC is incorrect (v2+ header)"];
	    }
	}

      n_trans = GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_timecnt);
      n_types = GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_typecnt);
      charcnt = GSSwapBigI32ToHost(*(int32_t*)(void*)header->tzh_charcnt);

      i = pos;
      if (1 == version)
	{
	  /* v1 with 32-bit transitions */
	  i += sizeof(int32_t) * n_trans;
	}
      else
	{
	  /* v2+ with 64-bit transitions */
	  i += sizeof(int64_t) * n_trans;
	}
      if (i > length)
	{
	  [NSException raise: fileException
		      format: @"Transitions list is truncated"];
	}
      i += n_trans;
      if (i > length)
	{
	  [NSException raise: fileException
		      format: @"Transition indexes are truncated"];
	}
      i += sizeof(struct ttinfo)*n_types;
      if (i > length)
	{
	  [NSException raise: fileException
		      format: @"Types list is truncated"];
	}
      if (i + charcnt > length)
	{
	  [NSException raise: fileException
		      format: @"Abbreviations list is truncated"];
	}

      /*
       * Now calculate size we need to store the information
       * for efficient access ... not the same saze as the data
       * we received (we always use 64bit transitions internally).
       */
      i = n_trans * (sizeof(int64_t)+1) + n_types * sizeof(TypeInfo);
      buf = NSZoneMalloc(NSDefaultMallocZone(), i);
      types = (TypeInfo*)buf;
      buf += (n_types * sizeof(TypeInfo));
      trans = (int64_t*)buf;
      buf += (n_trans * sizeof(int64_t)); 
      idxs = (unsigned char*)buf;

      /* Read in transitions. */
      if (1 == version)
	{
	  /* v1 with 32-bit transitions */
	  for (i = 0; i < n_trans; i++)
	    {
	      trans[i] = GSSwapBigI32ToHost(*(int32_t*)(bytes + pos));
	      pos += sizeof(int32_t);
	    }
	}
      else
	{
	  /* v2+ with 64-bit transitions */
	  for (i = 0; i < n_trans; i++)
	    {
	      trans[i] = GSSwapBigI64ToHost(*(int64_t*)(bytes + pos));
	      pos += sizeof(int64_t);
	    }
	}
      for (i = 0; i < n_trans; i++)
	{
	  idxs[i] = *(unsigned char*)(bytes + pos);
	  pos++;
	}
      for (i = 0; i < n_types; i++)
	{
	  struct ttinfo	*ptr = (struct ttinfo*)(bytes + pos);
          uint32_t      off;

	  types[i].isdst = (ptr->isdst != 0 ? YES : NO);
	  types[i].abbr_idx = ptr->abbr_idx;
          memcpy(&off, ptr->offset, 4);
	  types[i].offset = GSSwapBigI32ToHost(off);
	  pos += sizeof(struct ttinfo);
	}
      abbr = (unsigned char*)(bytes + pos);
      {
	id		abbrevs[charcnt];
	unsigned	count = 0;
	unsigned	used = 0;

	memset(abbrevs, '\0', sizeof(id)*charcnt);
	for (i = 0; i < n_types; i++)
	  {
	    int	loc = types[i].abbr_idx;

	    if (abbrevs[loc] == nil)
	      {
		abbrevs[loc]
		  = [[NSString alloc] initWithUTF8String: (char*)abbr + loc];
		count++;
	      }
	    types[i].abbreviation = abbrevs[loc];
	  }
	/*
	 * Now we have created all the abbreviations, we put them in an
	 * array for easy access later and easy deallocation if/when
	 * the receiver is deallocated.
	 */
	i = charcnt;
	while (i-- > count)
	  {
	    if (abbrevs[i] != nil)
	      {
		while (abbrevs[used] != nil)
		  {
		    used++;
		  }
		abbrevs[used] = abbrevs[i];
		abbrevs[i] = nil;
		if (++used >= count)
		  {
		    break;
		  }
	      }
	  }
	abbreviations = [[NSArray alloc] initWithObjects: abbrevs count: count];
	while (count-- > 0)
	  {
	    RELEASE(abbrevs[count]);
	  }
      }

      pthread_mutex_lock(&zone_mutex);
      [zoneDictionary setObject: self forKey: timeZoneName];
      pthread_mutex_unlock(&zone_mutex);
    }
  NS_HANDLER
    {
      DESTROY(self);
      NSLog(@"Unable to obtain time zone `%@'... %@", name, localException);
      if ([localException name] != fileException)
	{
	  [localException raise];
	}
    }
  NS_ENDHANDLER
  return self;
}

- (BOOL) isDaylightSavingTimeForDate: (NSDate*)aDate
{
  TypeInfo	*type = getTypeInfo([aDate timeIntervalSince1970], self);

  return type->isdst;
}

- (NSString*) name
{
  return timeZoneName;
}

- (NSInteger) secondsFromGMTForDate: (NSDate*)aDate
{
  TypeInfo	*type = getTypeInfo([aDate timeIntervalSince1970], self);

  return type->offset;
}

- (NSArray*) timeZoneDetailArray
{
  NSTimeZoneDetail	*details[n_types];
  unsigned		i;
  NSArray		*array;

  for (i = 0; i < n_types; i++)
    {
      details[i] = newDetailInZoneForType(self, &types[i]);
    }
  array = [NSArray arrayWithObjects: details count: n_types];
  for (i = 0; i < n_types; i++)
    {
      RELEASE(details[i]);
    }
  return array;
}

- (NSTimeZoneDetail*) timeZoneDetailForDate: (NSDate*)aDate
{
  TypeInfo		*type;
  NSTimeZoneDetail	*detail;

  type = getTypeInfo([aDate timeIntervalSince1970], self);
  detail = newDetailInZoneForType(self, type);
  return AUTORELEASE(detail);
}

- (NSString*) timeZoneName
{
  return timeZoneName;
}

@end

