# PSYC 5301 - Lab 1.1
# R assignment
#
# Instructions: on line 9, change the value of "studentID" to your own Tarleton ID.
# On line 10, change the value of "openSesameNum" to the number you got from OpenSesame
# Then execute the code on lines 9-14.  The answer will be a number that will
# appear in the console.  Submit that number in Canvas.

studentID = 000111222
openSesameNum = 9999

set.seed(studentID)
x = rnorm(n=1, mean=openSesameNum, sd=500)
round(x, digits=2)
