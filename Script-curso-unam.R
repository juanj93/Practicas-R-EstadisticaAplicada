#CURSO DE R  DE LA UNAM
install.packages("curl", dependencies = TRUE)
install.packages("httr", dependencies = TRUE)


install.packages("swirl", dependencies = TRUE)

library("swirl")

select_language()

install_course_github("ifunam", "programacion-estadistica-r")

Sys.setlocale("LC_ALL", "en_US.UTF-8")

swirl()
