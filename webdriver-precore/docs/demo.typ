
#import "@preview/diagraph:0.3.0": *


#outline(title: "Contents")

= Heading

this is an example

== heading 2

#let user_profile = "the user profile is Hello World"

#let log_in(userName, password) = (
  "Log in with: " 
    + userName 
    + "password: " 
    + password
  )

#let home = "https://example.com/home"

The user profile is #user_profile
+ NAVIGATE TO #home
  + submit form
  + click button
+ #log_in("myUser", "myPassword")

#image("image.png")

#circle(radius: 25pt)

#render("digraph { A -> B; B -> C; B -> D; A [label=\"Start\"]; B [label=\"Decision\"]; C [label=\"Result 1\"]; D [label=\"Result 2\"] }")

#table(
["Name", "Age", "Occupation"],
[
  ["Alice", "30", "Engineer"],
  ["Bob", "25", "Designer"],
  ["Charlie", "35", "Teacher"]
]
)



