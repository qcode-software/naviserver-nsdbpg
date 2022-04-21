/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://mozilla.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AOLserver Code and related documentation
 * distributed by AOL.
 *
 * The Initial Developer of the Original Code is America Online,
 * Inc. Portions created by AOL are Copyright (C) 1999 America Online,
 * Inc. All Rights Reserved.
 *
 * Alternatively, the contents of this file may be used under the terms
 * of the GNU General Public License (the "GPL"), in which case the
 * provisions of GPL are applicable instead of those above.  If you wish
 * to allow use of your version of this file only under the terms of the
 * GPL and not to allow others to use your version of this file under the
 * License, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the GPL.
 * If you do not delete the provisions above, a recipient may use your
 * version of this file under either the License or the GPL.
 *
 */

/*
 * dbpg.h --
 *
 *      Private types and declarations for the nsdbpg module.
 */

#ifndef DBPG_H
#define DBPG_H

#define NSDBPG_VERSION "2.6"

/*
 * In order to obtain PG_VERSION_NUM and PG_VERSION we load the
 * pg_config.h. However, the PACKAGE_* macros conflict with
 * NaviServer's packaging information, so we drop these.
 */
#include <pg_config.h>
#undef PACKAGE_VERSION
#undef PACKAGE_TARNAME
#undef PACKAGE_STRING
#undef PACKAGE_NAME
#undef PACKAGE_BUGREPORT
#undef PACKAGE_URL

#include <nsdb.h>
#include <libpq-fe.h>

/*
 * The following structure maintains per handle data
 * specific to postgres.
 */ 

typedef struct Connection {
    PGconn         *pgconn;
    PGresult       *res;
    unsigned int    id;
    int             nCols;
    int             nTuples;
    int             curTuple;
    Ns_Time         transactionStartTime;
    bool            in_transaction;
} Connection;

extern const char *pgDbName;
extern int Ns_PgServerInit(const char *server, const char *module, const char *driver);


#endif /* DBPG_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * indent-tabs-mode: nil
 * End:
 */
