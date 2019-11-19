#lang racket

(require net/url
         db
         json
         web-server/servlet
         web-server/servlet-env
         racket/date)

(define USER_ID "63E014187E0DAE7818EE2AAB125B4269")

;;;;;;;;;;;;;;;;;; Tools ;;;;;;;;;;;;;;;;
;; Open the sqlite3 connection:
(define sql-conn
  (sqlite3-connect #:database "qing-dao-beer.db"
                   #:use-place #f))
(query-exec sql-conn
            "CREATE TABLE IF NOT EXISTS Phone (id integer PRIMARY KEY AUTOINCREMENT, phone VARCHAR(11) UNIQUE)")

;; 获取图片验证码
(define get-code
  (lambda ()
    (let* ([value (call/input-url (string->url "http://149.129.61.211:8089/getCode")
                    get-pure-port
                    (lambda (port)
                      (with-input-from-string
                        (port->string port)
                        (lambda () (read-json)))))]
           [code (hash-ref value 'msg)])
      (substring code 0 (- (string-length code) 2)))))


;;;;;;;;;;;;;;;;;; Servlets ;;;;;;;;;;;;;;;;
(define (main request)
  (render-home-page request))
(define (render-home-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "青岛啤酒领流量系统")
                  ;; Css link
                  (link ([rel "stylesheet"]
                         [href "/main.css"]
                         [type "text/css"])))
            (body (h2 "添加号码")
                  (form ([action ,(embed/url add-phone)])
                        ;; Phone number input
                        "手机号："
                        (input ([name "phone"]
                              [type "text"]
                              [placeholder "Your phone number"]))
                      (br)
                      ;; Add sumbit
                      (input ([type "submit"]
                              [id "add-phone"]
                              [value "添加自动挂机"])))))))
  (define (add-phone request)
    (let* ([bindings (request-bindings request)]
           [phone-number (extract-binding/single 'phone bindings)])
      ;; 测试手机号的规范
      (if (regexp-match? #px"^1[3-9]\\d{9}$" phone-number)
          (if (add-to-database phone-number)
              (add-success phone-number request)
              (add-exists phone-number request))
          (add-failed phone-number request))))
  
  ;; Add success
  (define (add-success number request)
    (response/xexpr
     `(html (head (title "添加成功")
                  ;; Css link
                  (link ([rel "stylesheet"]
                         [href "/main.css"]
                         [type "text/css"])))
            (body
             (h3 ,(string-append "手机号:`" number "`添加成功~!"))))))
  
  ;; Add failed
  (define (add-failed number request)
     (response/xexpr
     `(html (head (title "添加失败")
                  ;; Css link
                  (link ([rel "stylesheet"]
                         [href "/main.css"]
                         [type "text/css"])))
            (body
             (h3 ,(string-append "添加失败：`" number "`不是正确的手机号。"))))))
  
  ;; Aready exists
  (define (add-exists number request)
    (response/xexpr
     `(html (head (title "添加提示")
                  ;; Css link
                  (link ([rel "stylesheet"]
                         [href "/main.css"]
                         [type "text/css"])))
            (body
             (h3 ,(string-append "手机号`" number "`已经添加过，请勿重复添加。"))))))
  (send/suspend/dispatch response-generator))


;; Databases area
(define add-to-database
  (lambda (number)
    (with-handlers ([exn:fail:sql?
                     (lambda (exn) #f)])
      (query-exec sql-conn "INSERT INTO Phone(phone) VALUES ($1)"
                  number)
      #t)))

;;;;;;;;;;;;;;;;;;; Deploy Servlets ;;;;;;;;;;;;;;;;;;;;
(serve/servlet main
               #:launch-browser? #f
               #:port 8099
               #:listen-ip #f
               #:servlet-path "/"
               #:extra-files-paths
               (list (build-path "statics")))

(define (validate-code number code)
  (let* ([content
         (call/input-url (string->url "https://m.client.10010.com/sma-lottery/qpactivity/qpLuckdraw.htm")
                         (lambda (url)
                           (post-pure-port url
                                           (string-append "mobile=" number "&image=" code "&userid=" USER_ID))
                           port->string))]
         [result (with-input-from-string content read-json)])
    (if (eq? (hash-ref 'code) "YES")
        (hash-ref 'mobile)
        '())))

;; Get web content
(define (get-content number hex-code)
  (let ([content
         (call/input-url (string->url "https://m.client.10010.com/sma-lottery/qpactivity/qpLuckdraw.htm")
                  (lambda (url)
                    (post-pure-port url
                                    (string-append "mobile=" number "&image=" hex-code "&userid=" USER_ID))
                    port->string))])
    (hash-ref content 'status)))

;; Draw
(define (draw)
  (for ([(index number) (in-query sql-conn "SELECT * FROM Phone")]
        [code (get-code)])
    (if (= (string-length code) 4)
        (let* ([hex-code (validate-code number code)]
               [status (get-content number hex-code)])
          (case status
            [(000 200) '抽奖成功]
            [(500) '没抽奖次数了哦，改日再战吧!]
            [(400) '当前参与人数众多，请稍后再试！]
            [(700) '当前抽奖人数过多，请稍后重试！]))
        (display "重新获取验证码"))
    ;; sleep 2 second
    (sleep 2)))


;; Timer
(let loop ()
  (let ([hour (date-hour (current-date))]
        [minute (date-minute (current-date))])
    (when (and (= hour 0) (= minute 10))
      (draw))
    ;; sleep for 1 minute
    (sleep 60)
    (loop)))

