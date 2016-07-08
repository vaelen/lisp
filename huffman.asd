(asdf:defsystem :huffman
  :version "0.0.1"
  :description "A Huffman Encoder"
  :author "Andrew Young <andrew@vaelen.org>"
  :licence "GPLv3"
  :serial t
  :depends-on (:cl-heap)
  :components ((:file "huffman")))
