#lang racket

(define (main-regex str)
  (regexp-match #px"select (?:(distinct) (.*)|(MAX)\\((\\w*)\\)|(COUNT)\\((\\w*)\\)|(AVG)\\((\\w*)\\)|(.*)) (from (?:\\w*))(?:\\s)?(where\\([\\w\\d\\s=,]*\\)|orderby\\([\\w\\d=,]*\\))?(?:\\s)?(orderby\\([\\w\\d=,]*\\))?" str))

(main-regex "select AVG(row) from table where(row=1 and col=2) orderby(row,row)")
