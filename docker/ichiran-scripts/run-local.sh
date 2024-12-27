#!/bin/bash

echo "Starting web server..."
sbcl --eval "(push #p\"$HOME/quicklisp/local-projects/\" ql:*local-project-directories*)" \
     --eval "(ql:register-local-projects)" \
     --eval "(ql:quickload :ichiran)" \
     --eval "(ql:quickload :ichiran/web)" \
     --eval "(format t \"~%Adding caches...~%\")" \
     --eval "(in-package :ichiran/all)" \
     --eval "(init-all-caches)" \
     --eval "(ichiran/web:start-server)" \
     --eval "(loop (sleep 1))"
     
echo "Starting web server..."
