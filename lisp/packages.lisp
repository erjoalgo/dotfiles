(defpackage erjoalgo-stumpmwrc/util
  (:use :cl)
  (:import-from #:postmodern
                connect-toplevel
                query
                dao-class
                dao-table-definition
                db-null
                execute
                insert-dao
                update-dao
                save-dao
                sql
                defprepared
                with-connection)
  (:export #:trim-spaces))
