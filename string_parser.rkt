#lang racket
;;Major assignment 5

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

;;pred-p takes a predicate p and checks if the
;first char of string satisfies the predicate
(define (pred-p p)
  (lambda (str)
    (let*([lchar (string->list str)]
          [firstchr (car lchar)])
      (if (p firstchr)
          (cons firstchr (list->string (cdr lchar)))
          'fail))))


;;;basic parsers to be used in big parsers;;;;;
;;single-digit-p using pred-p to check for the first char in string
(define single-digit-p (lambda (str)
                           ((pred-p (lambda (x)
                                 (let*([asci (char->integer x)])
                                   (if (and (>= asci 48) (<= asci 57))
                                       #t
                                       #f)))) str)))
;;single-alphabet-p using pred-p to check for the first char in string
(define single-alphabet-p (lambda (str)
                           ((pred-p (lambda (x)
                                      (char-alphabetic? x))) str)))
(define single-alphanumeric-p (lambda (str)
                         ((pred-p (lambda (x)
                                    (or (char-numeric? x) (char-alphabetic? x)))) str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;some special character detectors;;;;;;;;;

;;function to detect '[' in a string
(define (is-there-sq-bracket str)
  (sq-bracket-helper str ""))
(define (sq-bracket-helper c_string string_passed)
  (if (string=? c_string "")
      #f
      (let*([c-list (string->list c_string)]
            [passed-list (string->list string_passed)]
            [f-char (car c-list)])
        (if (char=? f-char #\[)
            #t
            (let*([c-list (cdr c-list)]
                  [c_string (list->string c-list)]
                  [passed-list (append passed-list (list f-char))]
                  [string_passed (list->string passed-list)])
              (sq-bracket-helper c_string string_passed))))))

;;function to detect ']' in a string
(define (is-there-lsq-bracket str)
  (lsq-bracket-helper str ""))
(define (lsq-bracket-helper c_string string_passed)
  (if (string=? c_string "")
      #f
      (let*([c-list (string->list c_string)]
            [passed-list (string->list string_passed)]
            [f-char (car c-list)])
        (if (char=? f-char #\])
            #t
            (let*([c-list (cdr c-list)]
                  [c_string (list->string c-list)]
                  [passed-list (append passed-list (list f-char))]
                  [string_passed (list->string passed-list)])
              (lsq-bracket-helper c_string string_passed))))))

;;function to detect '(' in a string
(define (is-there-c-bracket str)
  (c-bracket-helper str ""))
(define (c-bracket-helper c_string string_passed)
  (if (string=? c_string "")
      #f
      (let*([c-list (string->list c_string)]
            [passed-list (string->list string_passed)]
            [f-char (car c-list)])
        (if (char=? f-char #\()
            #t
            (let*([c-list (cdr c-list)]
                  [c_string (list->string c-list)]
                  [passed-list (append passed-list (list f-char))]
                  [strnig-passed (list->string passed-list)])
              (c-bracket-helper c_string string_passed))))))

;;function to detect ')' in a string
(define (is-there-lc-bracket str)
  (lc-bracket-helper str ""))
(define (lc-bracket-helper c_string string_passed)
  (if (string=? c_string "")
      #f
      (let*([c-list (string->list c_string)]
            [passed-list (string->list string_passed)]
            [f-char (car c-list)])
        (if (char=? f-char #\))
            #t
            (let*([c-list (cdr c-list)]
                  [c_string (list->string c-list)]
                  [passed-list (append passed-list (list f-char))]
                  [strnig-passed (list->string passed-list)])
              (lc-bracket-helper c_string string_passed))))))

;;function to detect '+' in a string
(define (is-there-+ str)
  (+-helper str ""))
(define (+-helper c_string string_passed)
  (if (string=? c_string "")
      #f
      (let*([c-list (string->list c_string)]
            [passed-list (string->list string_passed)]
            [f-char (car c-list)])
        (if (char=? f-char #\+)
            #t
            (let*([c-list (cdr c-list)]
                  [c_string (list->string c-list)]
                  [passed-list (append passed-list (list f-char))]
                  [strnig-passed (list->string passed-list)])
              (+-helper c_string string_passed))))))

;;function to detect '=' in a string
(define (is-there-= str)
  (=-helper str ""))
(define (=-helper c_string string_passed)
  (if (string=? c_string "")
      #f
      (let*([c-list (string->list c_string)]
            [passed-list (string->list string_passed)]
            [f-char (car c-list)])
        (if (char=? f-char #\=)
            #t
            (let*([c-list (cdr c-list)]
                  [c_string (list->string c-list)]
                  [passed-list (append passed-list (list f-char))]
                  [strnig-passed (list->string passed-list)])
              (=-helper c_string string_passed))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;some useful auxillary definitions;;;;;;;
(define (combine-cc char1 char2)
(list->string (list char1 char2)))

(define (combine-sc str char)
  (list->string (append (string->list str)
                        (list char))))

(define (combine-cs char str)
  (list->string (cons char (string->list str))))

(define (combine-ss str1 str2)
  (list->string (append (string->list str1)
                        (string->list str2))))

;;a function which would input a string with no leading and preceding " "
;;and would also remove any intermediate " "
(define (space-remover str)
  (space-helper (string->list str) '()))
(define (space-helper c_list passed_list)
  (if (null? c_list)
      (list->string passed_list)
      (if (char=? (car c_list) #\space)
          (space-helper (cdr c_list) passed_list)
          (let*([passed_list (append passed_list (list (car c_list)))])
            (space-helper (cdr c_list) passed_list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;Not so basic parsers;;;;;;;;;;;;;;;;;;;;;  
(define (seq parse1 parse2 combine-cc)
  (lambda (str)
    (let*([firstparse (parse1 str)]
          [res1 (car firstparse)]
          [secondparse (parse2 (cdr firstparse))]
          [res2 (car secondparse)]
          [res_car (combine-cc res1 res2)]
          [res_cdr (cdr secondparse)]
          [res (cons res_car res_cdr)])
      res)))

(define (alt parse1 parse2)
  (lambda (str)
    (if (eq? (parse1 str) 'fail)
        (parse2 str)
        (parse1 str))))

(define epsilon-p (lambda (str)
                    (cons "" str)))

(define (zero-or-more p f) (lambda (str) ;f in question is combined-cs(not sc) hence i have to use reverse in last
                             (define (helper-loop s_parsed s_remaining)
                               (if (string=? s_remaining "") ;subtlity eq? doesn't work
                                   (cons (list->string (reverse (string->list s_parsed))) s_remaining)
                                   (if (eq? (p s_remaining) 'fail)
                                       (cons (list->string (reverse (string->list s_parsed))) s_remaining)
                                       (let*([cres (p s_remaining)]
                                             [c (car cres)]
                                             [s_parsed (f c s_parsed)]
                                             [s_remaining (cdr cres)])
                                         (helper-loop s_parsed s_remaining)))))
                             (helper-loop "" str)))

;;whitspace-p can be made using zero-or-more applying a predicate checking
;;for #\space at the beginning unitill we run out of it
(define single-whitespace-p (lambda (str)
                              ((pred-p (lambda (x)
                                        (char=? x #\space))) str)))
(define (whitespace-p str)
  (define whitespace-p-helper (zero-or-more single-whitespace-p combine-cs))
  (let*([cres (whitespace-p-helper str)]);;this won't be final ans, I have to keep the first value of cons as empty string
    (cons "" (cdr cres))))

(define number-p (lambda (str)
                   (let*([without_space (whitespace-p str)]
                         [s (cdr without_space)];;now I can pass this s to a zero-or-more number-p
                         [ctemp ((zero-or-more single-digit-p combine-cs) s)]
                         [num_s (car ctemp)]
                         [other_s (cdr ctemp)]
                         [n (num (string->number num_s))])
                     (cons n other_s))))

;;The only condition for identifier is that it should start with a letter and should not contain special characters
(define identifier-p (lambda (str)
                        (let*([without_space (whitespace-p str)]
                              [s (cdr without_space)])
                          (if (not (eq? (single-alphabet-p s) 'fail))
                              (let*([ctemp ((zero-or-more single-alphanumeric-p combine-cs) s)]
                                    [idn_s (car ctemp)]
                                    [other_s (cdr ctemp)])
                                (cons (ident idn_s) other_s))
                              (cons (ident "") s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;interdependent parsers;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;variable-parser;;;;;;;;;;;;;;;;;;;;;;;;;;
;;if it is an identifier definitely it is variable
;;if it is an identifier and has a '[' then I need to parse it further
;;if it is not even an identifier then it can't be variable
(define variable-p (lambda (str)
                     ;suppose identifier-p is a finction which returns me a cons where first
                     ;is an identifier and the second part is the part of string such that
                     ;the starting letter is a special character
                     (let*([s (cdr (whitespace-p str))]
                           [s (space-remover s)]
                           [after_iden (identifier-p s)])
                       (if (string=? (ident-str (car after_iden)) "") ;not identifier, can't be a variable
                           after_iden
                           (if (string=? (cdr after_iden) "") ;pure identifier
                               after_iden
                               (if (and (char=? #\[ (car (string->list (cdr after_iden)))) (char=? #\] (last (string->list (cdr after_iden))))) ;identifier with array
                                   ;I need to strip the first '[' and last ']' to parse the inner exprsn
                                   (let*([l (string->list (cdr after_iden))]
                                         [l1 (cdr l)] ;first '[' stripped
                                         [l2 (reverse (cdr (reverse l1)))] ;last ']' stripped
                                         [innr_expr (list->string l2)] ;now i can process the expresson
                                         [processed_expr (car (expression-p innr_expr))]
                                         [out (gnode 'ARRAY (list (car after_iden) processed_expr))])
                                     (cons out ""))
                                   (if (and (char=? #\[ (car (string->list (cdr after_iden)))) (not (char=? #\] (last (string->list (cdr after_iden))))))
                                       (let*([ctemp (f-sq-bracket-extract (cdr after_iden))]
                                             [sq-bracketed-term (car ctemp)]
                                             [l (string->list sq-bracketed-term)]
                                             [l1 (cdr l)]
                                             [l2 (remove (last l1) l1)]
                                             [innr_expr (list->string l2)]
                                             [processed_expr (car (expression-p innr_expr))]
                                             [other_s (cdr ctemp)]
                                             [out (gnode 'ARRAY (list (car after_iden) processed_expr))])
                                         (cons out other_s))
                                       after_iden)))))))

;;f-sq-bracket-extract is a function which takes input a string and returns (cons x y) where x is
;;that part of string which opens first with '[' and y is rest of the parts
(define (f-sq-bracket-extract str)
  (f-sq-helper (string->list str) '() 0 1))
(define (f-sq-helper c_list p_list rsqcount rsq0count)
  (if (char=? #\[ (car c_list))
      (let*([rsqcount (+ rsqcount 1)]
            [p_list (append p_list (list (car c_list)))]
            [c_list (cdr c_list)])
        (f-sq-helper c_list p_list rsqcount rsq0count))
      (if (char=? #\] (car c_list))
          (let*([rsqcount (- rsqcount 1)]
                [p_list (append p_list (list (car c_list)))]
                [c_list (cdr c_list)])
            (if (= rsqcount 0)
                (let*([rsq0count (+ rsq0count 1)])
                  (if (= rsq0count 2)
                      (cons (list->string p_list) (list->string c_list))
                      (f-sq-helper c_list p_list rsqcount rsq0count)))
                (f-sq-helper c_list p_list rsqcount rsq0count)))
          (let*([p_list (append p_list (list (car c_list)))]
                [c_list (cdr c_list)])
            (f-sq-helper c_list p_list rsqcount rsq0count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;term parser;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;term will either be pure number or pure varible or pure expression inside c-brackets
(define term-p (lambda (str)
                 ;first checking for the pure number
                 (let*([s (cdr (whitespace-p str))]
                       [s (space-remover s)]
                       [after_num (number-p s)])
                   (if (not (eq? (num-val (car after_num)) #f))
                       after_num
                       (let*([after_var (variable-p s)])
                         (if (gnode? (car after_var))
                             after_var
                             (if (not (equal? (ident-str (car after_var)) ""))
                                 after_var
                                 (if (and (char=? #\( (car (string->list s))) (char=? #\) (last (string->list s)))); potential expression inside c-brackets
                                     ;I need to strip the first '('and the last ')'
                                     (let*([l (string->list s)]
                                           [l1 (cdr l)] ;first '(' stripped
                                           [l2 (reverse (cdr (reverse l1)))] ;last ')' stripped 
                                           [expr (list->string l2)]
                                           [after_expr (expression-p expr)])
                                       after_expr)
                                     (if (and (char=? #\( (car (string->list s))) (not (char=? #\) (last (string->list s)))))
                                         ;then i have should parse it to the most outer bracket
                                         (let*([ctemp (last-c-bracket-extract s)]
                                               [c-bracketed-term (car ctemp)]
                                               [l (string->list c-bracketed-term)]
                                               [l1 (cdr l)]
                                               [l2 (remove (last l1) l1)]
                                               [innr_expr (list->string l2)]
                                               [processed_expr (car (expression-p innr_expr))]
                                               [other_s (cdr ctemp)])
                                           (cons processed_expr other_s))
                                         after_var)))))))))

;;f-c-bracket-extract is a function which will take a string and return (cons x y) where x is
;;that part of string which opens first with '(' bracket
(define (f-c-bracket-extract str)
  (f-c-helper (string->list str) '() 0 1))
(define (f-c-helper c_list p_list rccount rc0count)
  (if (char=? #\( (car c_list))
      (let*([rccount (+ rccount 1)]
            [p_list (append p_list (list (car c_list)))]
            [c_list (cdr c_list)])
        (f-c-helper c_list p_list rccount rc0count))
      (if (char=? #\) (car c_list))
          (let*([rccount (- rccount 1)]
                [p_list (append p_list (list (car c_list)))]
                [c_list (cdr c_list)])
            (if (= rccount 0)
                (let*([rc0count (+ rc0count 1)])
                  (if (= rc0count 2)
                      (cons (list->string p_list) (list->string c_list))
                      (f-c-helper c_list p_list rccount rc0count)))
                (f-c-helper c_list p_list rccount rc0count)))
          (let*([p_list (append p_list (list (car c_list)))]
                [c_list (cdr c_list)])
            (f-c-helper c_list p_list rccount rc0count)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;experssion-parser;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;expression will either be a term or term+expr+expr+expr+....
(define expression-p (lambda (str)
                       (let*([s (cdr (whitespace-p str))]
                             [s (space-remover s)]
                             [after_term (term-p s)])
                         (if (equal? (car after_term) "")
                             after_term
                             (if (not (string=? (cdr after_term) ""))
                                 (let*([f_term (car after_term)] ;I need to strip the first '+' from the rest of the string to check for the exprsn
                                       [l (string->list (cdr after_term))]
                                       [l1 (cdr l)] ; '+' stripped
                                       [remaining_expr (list->string l1)]
                                       [remaining_expr (car (expression-p remaining_expr))]
                                       [out (gnode 'PLUS (list f_term remaining_expr))])
                                   (cons out ""))
                                 after_term)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;assignment-parser;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;assignment parser would check for an expression before '=' and expression after '='
;;offcourse there has to be an '=' and only one
(define assignment-p (lambda (str)
                       (let*([s (cdr (whitespace-p str))]
                             [s (space-remover s)])
                         (if (is-there-= s)
                             (let*([ctemp (=-extract s "")]
                                   [lexpr (car ctemp)]
                                   [rexpr (cdr ctemp)]
                                   [lexpr_eval (car (expression-p lexpr))]
                                   [rexpr_eval (car (expression-p rexpr))]
                                   [out (gnode 'ASSIGN (list lexpr_eval rexpr_eval))])
                               (cons out ""))
                             'assign_fail))))
;;this function will take a string of form "x=y" as an input part and return (cons x y)
(define (=-extract c_string passed_string)
  (if (is-there-= c_string)
      (=-extract (list->string (cdr (string->list c_string)))
                            (list->string (append (string->list passed_string) (list (car (string->list c_string))))))
      (cons (list->string (remove #\= (string->list passed_string)))
                          (list->string (string->list c_string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                     
                         
                       
                                   
                                    
                                     
                                     
                                 
                               
                           
                           
                           
                           
                     
                     
                     
                     
        
                                    

                              
                              
                              
                        
                     
                         
                         
                         
                         
                   
    
     
                       
                             

                                   
                                   
                             

      
                                    
                                      
                                 