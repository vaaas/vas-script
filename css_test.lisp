(macro (red) '((px 15) (px 0) (px 15) (px 0)))

(rule (c html body header (> h1 h2 h3))
    margin ((red)))
