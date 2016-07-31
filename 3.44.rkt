(defein (serialized-exchange account1 account2)
  (let ((serializer1 (account1 `serializer))
        (serializer2 (account2 `serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define (transfer from-acount to-account amount)
  ((form-account `withdraw) amount)
  ((to-account `deposit) amount))