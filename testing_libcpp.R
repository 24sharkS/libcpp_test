abs1 <- ABS_Impl1$new(5L)
abs2 <- ABS_Impl2$new(9L)

# get the encapsulated integer.
abs1$get()
abs2$get()

i1 <- Int$new(10L)
i2 <- Int$new(i1)

# accessing integer value
i1$i_
i2$i_

i1$i_ <- 66L
i2$i_ <- 100L

i1$i_
i2$i_

# creating new objects.
l1 <- LibCppTest$new()
l2 <- LibCppTest$new(-100L)
l1$get()
l2$get()

# Testing the processes.

# twist
l1$twist(list("Shekhar",400L))

# process
vec <- c(3L,5L)
l1$process(vec)
all(vec == as.integer(c(3,5,42)))
l1$process(c(45L,65L))

# process2
vec <- c(10L,-1L)
l1$process2(vec)
all(vec == c(42L,11L))
l1$process2(c(10L,-1L))

# process3
pair <- list(LibCppTest$new(-67L),9L)
l1$process3(pair)
l1$process3(list(LibCppTest$new(-67L),9L))[[1]]$get()


# process 4
pair <- list(9L,LibCppTest$new(-67L))
l1$process4(pair)
l1$process4(pair)[[1]]
l1$process4(pair)[[2]]$get()
l1$process4(list(9L,LibCppTest$new(-67L)))[[1]]
l1$process4(list(9L,LibCppTest$new(-67L)))[[2]]$get()

# process 5
pair <- list(LibCppTest$new(99L),LibCppTest$new(300L))
l1$process5(pair)
l1$process5(list(LibCppTest$new(99L),LibCppTest$new(300L)))

# process 6 (reverses list after adding (7,11))
lp <- list(list(1L,3.14),list(45L,1.23),list(8L,1123123.91))
l1$process6(lp)
l1$process6(list(list(1L,3.14),list(45L,1.23),list(8L,1123123.91)))


# process 7
pair <- c(1L,4444L)
l1$process7(pair)
l1$process7(c(0L,4442L))

# process 8
vec <- as.integer(c(0L,0L,1L,1L,0L))
l1$process8(vec)

# process 9
seti <- c(1L,7L,9L,10L)
l1$process9(seti)
l1$process9(as.integer(c(10,101,102,103,104,150,160,107,108,109,110)))

# process 11
setl <- list(LibCppTest$new(10L),LibCppTest$new(42L))
l1$process11(setl)
l1$process11(list(LibCppTest$new(10L),LibCppTest$new(42L)))

# process 12
l1$process12(5L,3.44)

# process 13
l1$process13(1L,6L)

# process 14
l1$process14(1L,6L)

# process 15
l1$process15(77L)

# process 16
intf <- list(5.5,3.4,1.2)
names(intf) <- c(3L,99L,20L)
l1$process16(intf)
names(intf) <- c(42L,8L,3L)
l1$process16(intf)

# process 17
intf <- list(5.5,3.4)
names(intf) <- c(0L,1L)
l1$process17(intf)
names(intf) <- c(1L,0L)
l1$process17(intf)


# process 18
intf <- list(LibCppTest$new(),LibCppTest$new(-88L))
names(intf) <- c(-7L,23L)
l1$process18(intf)
names(intf) <- c(23L,-7L)
l1$process18(intf)

# process 19
intf <- list(LibCppTest$new(5677L),LibCppTest$new(200L))
names(intf) <- as.integer(c(3,4))
l1$process19(intf)
# this should silently return null
l1$process19(list("3" = LibCppTest$new(5677L),"4" = LibCppTest$new(200L)))
sapply(intf, function(x) x$get())

# process 20
intf <- list(4.056,3.445)
names(intf) <- 1:2
l1$process20(intf)
intf
intf <- list(4.056,3.445)
names(intf) <- 23:24
intf
l1$process20(intf)
intf
# this should silently return null
l1$process20(list("3" = 4.056,"6" = 3.445))


# process 21
intf <- list(3.4,4.5,6.7)
names(intf) <- 3:5
inti <- list(1L,3L,666L)
names(inti) <- 45:47
l1$process21(intf,inti)
# this should silently return null
l1$process21(list("3"=3.56,"4"=4.67),list("5"=4L,"7"=67L))

## process 211 to 214 fail as the python binding is not working.

# process 211
intf <- list(3.4,4.5,6.7)
names(intf) <- 3:5
str_vec <- list(as.integer(c(1,2,3)),as.integer(c(5,6)),as.integer(c(100,101)))
names(str_vec) <- c("A","B","C")
l1$process211(intf,str_vec)


# process 212
intf <- list(3.4,4.5,6.7)
names(intf) <- 3:5
str_vec <- list(list(as.integer(c(1,2)),as.integer(c(4,5))),list(c(-1L,-4L)))
names(str_vec) <- c("a","b")
l1$process212(intf,str_vec)

# process 213
intf <- list(3.4,4.5,6.7)
names(intf) <- 3:5
str_vec <- list(list(list(Int$new(4L),Int$new(-5L)),list(Int$new(1L),Int$new(3L))),list(list(Int$new(300L))))
names(str_vec) <- c("a","b")
l1$process212(intf,str_vec)


# process 214
intf <- list(3.4,4.5,6.7)
names(intf) <- 3:5
str_vec <- list(list(c(9L,10L),c(30L,2L)),list(c(-2L,8L),c(25L,2L)))
names(str_vec) <- c("a","b")
l1$process214(intf,str_vec)

# process 22
seti <- list(1L,1L,2L)
setf <- list(3.4,4.5,6.7)
# should throw error
l1$process22(seti,setf)
seti <- list(1L,3L,4L)
l1$process22(seti,setf)
l1$process22(list(1L,3L,4L),list(3.4,4.5,6.7))

# process 23
i <- list(1L,3L,4L,5L,6L)
f <- list(213.112,3.7,65.77,1.23)
l1$process23(i,f)
l1$process23(list(1L,3L,4L,5L,6L),list(213.112,3.7,65.77,1.23))

# process 24
intf <- list(1000L,3.14)
inti <- list(18L,81L)
l1$process24(intf,inti)

# process 25
vec_Int <- list(Int$new(3L),Int$new(9L),Int$new(101L))
l1$process25(vec_Int)
l1$process25(list(Int$new(3L),Int$new(9L),Int$new(101L)))

# process 26
vec_Int <- list(list(Int$new(3L)),list(Int$new(66L)),list(Int$new(101L)))
l1$process26(vec_Int)
l1$process26(list(list(Int$new(3L)),list(Int$new(66L)),list(Int$new(101L))))

# process 27
vec_Int <- list(list(list(Int$new(100L)),list(Int$new(-30L))),list(list(Int$new(200L)),list(Int$new(500L))))
l1$process27(vec_Int)

# process 28
vec_Int <- list(list(list(list(Int$new(100L)),list(Int$new(-30L))),list(list(Int$new(200L)),list(Int$new(500L)))),list(list(list(Int$new(100L)),list(Int$new(-30L))),list(list(Int$new(200L)),list(Int$new(500L)))))
l1$process28(vec_Int)

# process 29
vec_Int <- list(list(Int$new(7L)),list(Int$new(66L)),list(Int$new(101L)))
l1$process29(vec_Int)


# process 30
vec_Int <- list(list(list(list(Int$new(100L)),list(Int$new(-30L))),list(list(Int$new(200L)),list(Int$new(500L)))),list(list(list(Int$new(100L)),list(Int$new(-30L))),list(list(Int$new(200L)),list(Int$new(500L)))))
l1$process30(vec_Int)

# process 31
p31<-as.list(as.integer(c(1,2,3)))
l1$process31(p)

# process 32
p32 <- list(1:3,4:11,6:8)
l1$process32(p32)

# process 33
ob_Int <- Int$new(-1L)
l1$process33(ob_Int)

# process 34
ob_Int <- Int$new(4L)
l1$process34(ob_Int)

# process 35
ob_Int <- Int$new(44L)
l1$process35(ob_Int)$i_

# process 36
ob_Int <- Int$new(123L)
l1$process36(ob_Int)

# process 37
ob_Int <- Int$new(3L)
l1$process37(ob_Int)$i_
ob_Int <- Int$new(18L)
print(l1$process37(ob_Int))

# process 38
l1$process38(33L)

# process 39
ob_Int <- Int$new(999L)
l1$process39(ob_Int)$i_

# process 40
a1 <- ABS_Impl1$new(5L)
l1$process40(a1)
a2 <- ABS_Impl2$new(10L)
l1$process40(a2)
