;;; -*- lexical-binding: t; -*-

(map! :unless (display-graphic-p)
  "<mouse-4>" (cmd! (scroll-down 2))
  "<mouse-5>" (cmd! (scroll-up 2)))
