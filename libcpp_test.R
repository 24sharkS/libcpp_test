library(reticulate)
library(varhandle)
library(purrr)
library(R6)

libcpp <- import("py_libcpp_test")
py_gc <- import("gc")


ABS_Impl1 <- R6Class(classname = "ABS_Impl1",cloneable = FALSE,
                     private = list(
                       py_obj = NA
                     ),
                     
                     public = list(
                       initialize = function(i){
                           stopifnot(is_scalar_integer(i))
                           private$py_obj <- libcpp$ABS_Impl1(i)
                       },
                       
                       get = function(){
                         return(private$py_obj$get())
                       },
                       
                       get_py_object = function(){
                         return(private$py_obj)
                       },
                       
                     )                     
)


ABS_Impl2 <- R6Class(classname = "ABS_Impl2",cloneable = FALSE,
                     private = list(
                       py_obj = NA
                     ),
                     
                     public = list(
                       initialize = function(i){
                         stopifnot(is_scalar_integer(i))
                         private$py_obj <- libcpp$ABS_Impl2(i)
                       },
                       
                       get = function(){
                         return(private$py_obj$get())
                       },
                       
                       get_py_object = function(){
                         return(private$py_obj)
                       }
                     )
)


Int <- R6Class(classname = "Int",cloneable = FALSE,
               private = list(
                  py_obj = NA
               ),
               
               public = list(
                 initialize = function(i){
                   stopifnot(class(i) == c("Int","R6") || class(i) == "integer" && length(i)==1)
                   if(is.R6(i) && class(i)[1] == "Int"){
                     private$py_obj <- libcpp$Int(i$get_py_object())
                   } else{
                     private$py_obj <- libcpp$Int(i)
                   }
                 },
                 
                 get_py_object = function(){
                   return(private$py_obj)
                 }
               ),
               
               active = list(
                 i_ = function(value){
                   if(missing(value)){
                     return(private$py_obj$i_)
                   } else {
                     private$py_obj <- libcpp$Int(value)
                   }
                 }
               )
)

LibCppTest <- R6Class(classname = "LibCppTest",cloneable = FALSE,
                      private = list(
                        py_obj = NA
                      ),
                      
                      public = list(
                        
                        initialize = function(ii){
                          if(missing(ii)){
                            private$py_obj <- libcpp$LibCppTest()
                          } else if(!is_scalar_integer(ii)){
                            stop("wrong argument!!")
                          } else{
                            private$py_obj <- libcpp$LibCppTest(ii)
                          }
                        },
                        
                        
                        #' libcpp_pair[int,libcpp_string] twist(libcpp_pair[libcpp_string, int])
                        #' 
                        #' takes a list of character and integer as input.
                        twist = function(pair){
                          stopifnot(length(pair)==2 && is_scalar_character(pair[[1]]) && is_scalar_integer(pair[[2]]))
                          py$str <- pair[[1]]
                          py_run_string("str = bytes(str,'utf-8')")
                          ans <- private$py_obj$twist(list(py$str,pair[[2]]))
                          ans[[2]] <- as.character(ans[[2]])
                          py_run_string("del str")
                          py_gc$collect()
                          ans
                        },
                        
                        
                        #' libcpp_vector[int] process(libcpp_vector[int] &)
                        #' 
                        #' takes an integer vector of length 2 as input.
                        process = function(vec){
                          
                          stopifnot(is_integer(vec))
                          
                          vec1 <- r_to_py(vec)
                          
                          ans <- private$py_obj$process(vec1)
                          
                          vec1 <- py_to_r(vec1)
                          
                          tryCatch({
                            eval.parent(substitute(vec<-vec1))
                            return(ans)
                          },error = function(c) return(ans)
                          )
                          
                        },
                        
                        #' libcpp_pair[int, int] process2(libcpp_pair[int, int] &)
                        #' 
                        #' takes an integer vector of length 2 as input.
                        process2 = function(pair){
                          stopifnot(is_integer(pair) && length(pair)==2)
                          pair1 <- r_to_py(pair)
                          ans <- private$py_obj$process2(pair1)
                          pair1 <- py_to_r(pair1)
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)
                          },error = function(c) return(ans)
                          )
                        },
                        
                        #' libcpp_pair[LibCppTest, int] process3(libcpp_pair[LibCppTest, int] &)
                        #' 
                        #' takes a list of LibCppTest object and integer.
                        process3 = function(pair){
                          stopifnot(length(pair)==2 && all(class(pair[[1]])==c("LibCppTest","R6")) && is_scalar_integer(pair[[2]]))
                          
                          pair1 <- pair
                          pair1[[1]] <- pair1[[1]]$get_py_object()
                          pair1 <- r_to_py(pair1)
                          
                          ans <- private$py_obj$process3(pair1)
                          ans[[1]] <- LibCppTest$new(ans[[1]]$gett())
                          
                          pair1 <- py_to_r(pair1)
                          pair1[[1]] <- LibCppTest$new(pair1[[1]])
                          
                          
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)  
                          }, error = function(c) return(ans)
                          )
                          
                        },
                        
                        #' libcpp_pair[int, LibCppTest] process4(libcpp_pair[int, LibCppTest] &)
                        #' 
                        #' takes list of integer and LibCppTest object
                        process4 = function(pair){
<<<<<<< HEAD
                          stopifnot(length(pair)==2 && is_scalar_integer(pair[[1]]) && all(class(pair[[2]])==c("LibCppTest","R6")))
                          
                          pair1 <- pair
                          pair1[[2]] <- pair1[[2]]$get_py_object()
                          pair1 <- r_to_py(pair1)
||||||| merged common ancestors
                          pair[[2]] <- pair[[2]]$get_py_object()
=======
                          stopifnot(length(pair)==2 && is_scalar_integer(pair[[1]]) && all(class(pair[[2]])==c("LibCppTest","R6")))
                          
                          pair1 <- pair
                          pair1[[2]] <- pair1[[2]]$get_py_object()
                          pair1 <- r_to_py(pair1)
                          
                          ans <- private$py_obj$process4(pair1)
                          ans[[2]] <- LibCppTest$new(ans[[2]]$gett())
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
<<<<<<< HEAD
                          ans <- private$py_obj$process4(pair1)
                          ans[[2]] <- LibCppTest$new(ans[[2]]$gett())
                          
                          pair1 <- py_to_r(pair1)
                          pair1[[2]] <- LibCppTest$new(pair1[[2]]$gett())
                          
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)
                          }, error = function(c) return(ans))
||||||| merged common ancestors
                          pair2 <- private$py_obj$process4(pair)
=======
                          pair1 <- py_to_r(pair1)
                          pair1[[2]] <- LibCppTest$new(pair1[[2]]$gett())
                          
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)
                          }, error = function(c) return(ans))
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
                        },
                        
                        
                        
                        #' libcpp_pair[LibCppTest, LibCppTest] process5(libcpp_pair[LibCppTest, LibCppTest] &)
                        #' 
                        #' takes a list of LibCppTest objects.
                        process5 = function(pair){
<<<<<<< HEAD
                          stopifnot(length(pair)==2 && all(sapply(pair, function(f) all(class(f)==c("LibCppTest","R6")))))
                          pair1 <- pair
                          
                          pair1[[1]] <- pair1[[1]]$get_py_object()
                          pair1[[2]] <- pair1[[2]]$get_py_object()
||||||| merged common ancestors
                          pair[[1]] <- pair[[1]]$get_py_object()
                          pair[[2]] <- pair[[2]]$get_py_object()
=======
                          stopifnot(length(pair)==2 && all(sapply(pair, function(f) all(class(f)==c("LibCppTest","R6")))))
                          pair1 <- pair
                          
                          pair1[[1]] <- pair1[[1]]$get_py_object()
                          pair1[[2]] <- pair1[[2]]$get_py_object()
                          
                          pair1 <- r_to_py(pair1)
                          ans <- private$py_obj$process5(pair1)
                          ans[[1]] <- LibCppTest$new(ans[[1]]$gett())
                          ans[[2]] <- LibCppTest$new(ans[[2]]$gett())
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
<<<<<<< HEAD
                          pair1 <- r_to_py(pair1)
                          ans <- private$py_obj$process5(pair1)
                          ans[[1]] <- LibCppTest$new(ans[[1]]$gett())
                          ans[[2]] <- LibCppTest$new(ans[[2]]$gett())
                          
                          pair1 <- py_to_r(pair1)
                          pair1[[1]] <- LibCppTest$new(pair1[[1]]$gett())
                          pair1[[2]] <- LibCppTest$new(pair1[[2]]$gett())
                          
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)
                          }, error = function(c) return(ans))
||||||| merged common ancestors
                          pair2 <- private$py_obj$process5(pair)
=======
                          pair1 <- py_to_r(pair1)
                          pair1[[1]] <- LibCppTest$new(pair1[[1]]$gett())
                          pair1[[2]] <- LibCppTest$new(pair1[[2]]$gett())
                          
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)
                          }, error = function(c) return(ans))
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
                        },
                        
                        #' libcpp_vector[libcpp_pair[int,double]] process6(libcpp_vector[libcpp_pair[int,double]] &)
                        #' 
                        #' takes a list of list of integer and double type.
                        process6 = function(pair){
                          stopifnot(is_list(pair) && all(sapply(pair, function(x) is_scalar_integer(x[[1]]) && is_scalar_double(x[[2]]))))
                          
                          pair1 <- r_to_py(pair)
                          ans <- private$py_obj$process6(pair1)
                          pair1 <- py_to_r(pair1)
                          
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)
                          }, error = function(c) return(ans))
                        
                        },
                        
                        #' libcpp_pair[int, EEE] process7(libcpp_pair[EEE, int] &)
                        #' 
                        #' takes a vector of enum integer(0/1) and integer type.
                        process7 = function(pair){
                          stopifnot(length(pair)==2 && pair[1] %in% c(0,1) && class(pair[2])=="integer")
                          
                          pair1 <- r_to_py(pair)
                          ans <- private$py_obj$process7(pair)
                          pair1 <- py_to_r(pair1)
                          
                          tryCatch({
                            eval.parent(substitute(pair<-pair1))
                            return(ans)
                          }, error = function(c) return(ans))
                          
                        },
                        
                        #' libcpp_vector[EEE] process8(libcpp_vector[EEE] &)
                        #' 
                        #' takes a vector of enum integer(0/1).
                        process8 = function(vec){
                          stopifnot(is_integer(vec) && all(sapply(vec,function(x) x %in% c(1,0))))
                          
                          vec1 <- r_to_py(vec)
                          ans <- private$py_obj$process8(vec)
                          vec1 <- py_to_r(vec1)
                          
                          tryCatch({
                            eval.parent(substitute(vec<-vec1))
                            return(ans)
                          }, error = function(c) return(ans))
                          
                        },
                        
                        #' libcpp_set[int] process9(libcpp_set[int] &)
                        #'
                        #' takes an integer vector of unique values. 
                        process9 = function(seti){
                          
                          stopifnot( is_integer(seti) && !(TRUE %in% duplicated(seti)) )
                          
                          # create python list corresponding to R vector.
                          py$o1 <- seti
                          
                          # convert the python list to set.
                          py_run_string("o1 = set(o1)")
                          
                          # call the python function `process9` passing the set as argument.
                          # function returns a set also.
                          py$o2 <- py_call(private$py_obj$process9,py$o1)
                          
                          ans <- py_eval("list(o2)")
                          
                          tryCatch({
                            eval.parent(substitute(seti<-py_eval("list(o1)")))
                            py_run_string("del o1;del o2")
                            py_gc$collect()
                            return(ans)
                          }, error = function(c){
                            py_run_string("del o1;del o2")
                            py_gc$collect()
                            return(ans)
                          } )
                          
                        },
                        
                        #' libcpp_set[LibCppTest] process11(libcpp_set[LibCppTest] &)
                        #' 
                        #' takes a list of unique LibCppTest objects(we check through underlying python objects).
                        #' a <- LibCppTest$new(10L) 
                        #' b <- LibCppTest$new(10L)
                        #' Here, a & b cannot both be in the list since they are equal.
                        process11 = function(setl){
                          # check if all are LibCppTest object.
                          stopifnot(is_list(setl) && all(sapply(setl, function(x) all(class(x)==c("LibCppTest","R6")))))
                          
                          # check if all unique
                          if( TRUE %in% duplicated(sapply(setl, function(x) x$get())) ){
                            stop("argument values should be unique")
                          }
                          
<<<<<<< HEAD
                          # converting r to python object.
                          setl <- lapply(setl, function(x) x$get_py_object())
||||||| merged common ancestors
                          py$o1 <- input
=======
                          # converting r to python object.
                          setl <- lapply(setl, function(x) x$get_py_object())
                          
                          py$o1 <- setl
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
                          py$o1 <- setl
                          
<<<<<<< HEAD
                          py_run_string("o1 = set(o1)")
||||||| merged common ancestors
                          py$o1 <- py_call(private$py_obj$process11,py$o1)
=======
                          py$o2 <- py_call(private$py_obj$process11,py$o1)
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
<<<<<<< HEAD
                          py$o2 <- py_call(private$py_obj$process11,py$o1)
||||||| merged common ancestors
                          ans <- py_eval("list(o1)")
=======
                          ans <- py_eval("list(o2)")
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
<<<<<<< HEAD
                          ans <- py_eval("list(o2)")
||||||| merged common ancestors
                          py_run_string("del o1")
                          py_gc$collect()
=======
                          tryCatch({
                            eval.parent(substitute(setl<-py_eval("list(o1)")))
                            py_run_string("del o1;del o2")
                            py_gc$collect()
                            return(lapply(ans,function(x) LibCppTest$new(x$gett())))
                          }, error = function(c){
                            py_run_string("del o1;del o2")
                            py_gc$collect()
                            return(lapply(ans,function(x) LibCppTest$new(x$gett())))
                          } )
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
<<<<<<< HEAD
                          tryCatch({
                            eval.parent(substitute(setl<-py_eval("list(o1)")))
                            py_run_string("del o1;del o2")
                            py_gc$collect()
                            return(lapply(ans,function(x) LibCppTest$new(x$gett())))
                          }, error = function(c){
                            py_run_string("del o1;del o2")
                            py_gc$collect()
                            return(lapply(ans,function(x) LibCppTest$new(x$gett())))
                          } )
                          
||||||| merged common ancestors
                          ans <- lapply(ans,function(x) LibCppTest$new(x$gett()))
                          
                          return(ans)
=======
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                        },
                        
                        #' libcpp_map[int, float] process12(int i, float f)
                        #' 
                        #' takes two scalar vectors of type integer and double.
                        process12 = function(a,b){
                          stopifnot(is_scalar_integer(a) && is_scalar_double(b))
                          return(private$py_obj$process12(a,b))
                        },
                        
                        #' libcpp_map[EEE, int] process13(EEE e, int i)
                        #' 
                        #' takes two scalar vectors of enum integer(0/1) and integer type.
                        process13 = function(enum,int){
                          stopifnot(is_scalar_integer(enum) && enum %in% c(1,0) && is_scalar_integer(int))
                          return(private$py_obj$process13(enum,int))
                        },
                        
                        #' libcpp_map[int, EEE] process14(EEE e, int i)
                        #'
                        #' takes two scalar vectors of enum integer(0/1) and integer type.
                        process14 = function(enum,int){
                          stopifnot(class(enum)=="integer" && class(int)=="integer" && enum %in% c(1,0) && length(int)==1)
                          return(private$py_obj$process14(enum,int))
                        },
                        
                        #' libcpp_map[long int, LibCppTest] process15(int ii)
                        #' 
                        #' takes a scalar integer vector.
                        process15 = function(ii){
                          stopifnot(is_scalar_integer(ii))
                          
                          ans <- private$py_obj$process15(ii)
                          ans[[1]] <- LibCppTest$new()$set_py_object(ans[[1]])
                          return(ans)
                        },
                        
                        #' float process16(libcpp_map[int, float] in_)
                        #' 
                        #' takes a named list of double type with names as integer type.
                        process16 = function(dict){
                          # check if list is named, names of integer type, values of double type.
                          stopifnot(is_list(dict) && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) is_double(d))))
                          
                          # check that list names should be unique.
                          # R allows for different elements with same name.
                          if(!length(unique(names(dict))) == length(names(dict))) {
                            stop("List names should be unique")
                          }
                          
                          if(length(dict)==1){
                            py$key <- list(as.integer(names(dict)))
                            py$val <- unname(dict)
                          } else{
                            py$key <- as.integer(names(dict))
                            py$val <- unname(dict)
                          }
                          
                          ans <- py_to_r(py_call(private$py_obj$process16,py_eval("dict(zip(key,val))",convert = F))) 
                          
                          py_run_string("del key;del val")
                          
                          py_gc$collect()
                          
                          return(ans)
                        },
                        
                        #' float process17(libcpp_map[EEE, float] in_)
                        #' 
                        #' takes a named list of double type and names as enum integer(0/1).
                        process17 = function(dict){
                          
                          stopifnot(class(dict)=="list" && !is.null(names(dict)) && all(sapply(names(dict),function(d) d %in% c(1,0))) && all(sapply(dict,function(d) is_double(d))))
                          
                          
                          # check that list name should be unique.
                          if(!length(unique(names(dict))) == length(names(dict))) {
                            stop("List names should be unique")
                          }
                          
                          
                          if(length(dict)==1){
                            py$key <- list(as.integer(names(dict)))
                            py$val <- unname(dict)
                          } else{
                            py$key <- as.integer(names(dict))
                            py$val <- unname(dict)
                          }
                          
                          ans <- py_to_r(py_call(private$py_obj$process17,py_eval("dict(zip(key,val))",convert = F)))
                          
                          py_run_string("del key;del val")
                          
                          py_gc$collect()
                          
                          return(ans)
                          
                        },
                        
                        #' int process18(libcpp_map[int, LibCppTest] in_)
                        #' 
                        #' takes a named list of LibCppTest object and names as integer type.
                        process18 = function(dict){
                          
<<<<<<< HEAD
                          stopifnot(is_list(dict) && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) all(class(d)==c("LibCppTest","R6")))))
                          
                          # check that list name should be unique.
                          if(!length(unique(names(dict))) == length(names(dict))) {
                            stop("List names should be unique")
                          }
||||||| merged common ancestors
                          stopifnot(class(dict)=="list" && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) all(class(d)==c("LibCppTest","R6")))))
=======
                          stopifnot(is_list(dict) && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) all(class(d)==c("LibCppTest","R6")))))
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
<<<<<<< HEAD
                          dict <- lapply(dict, function(x) x$get_py_object())
||||||| merged common ancestors
                          dict <- lapply(dict, function(x) dict$get_py_object(x))
=======
                          # check that list name should be unique.
                          if(!length(unique(names(dict))) == length(names(dict))) {
                            stop("List names should be unique")
                          }
                          
                          dict <- lapply(dict, function(x) x$get_py_object())
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
                          if(length(dict)==1){
                            py$key <- list(as.integer(names(dict)))
                            py$val <- unname(dict)
                          } else{
                            py$key <- as.integer(names(dict))
                            py$val <- unname(dict)
                          }
                          
                          ans <- py_to_r(py_call(private$py_obj$process18,py_eval("dict(zip(key,val))",convert = F)))
                          
                          py_run_string("del key;del val")
                          
                          py_gc$collect()
                          
                          return(ans)
                          
                        },
                        
                        #' void  process19(libcpp_map[int, LibCppTest] & in_)
                        #' 
                        #' takes a named list of LibCppTest object and names as integer type.
                        process19 = function(dict){
                          
                          stopifnot(class(dict)=="list" && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) class(d)==c("LibCppTest","R6"))))
                          
                          # check that list name should be unique.
                          if(!length(unique(names(dict))) == length(names(dict))) {
                            stop("List names should be unique")
                          }
                          
                          dict1 <- dict
                          
                          for (i in sequence(length(dict))) {
                            dict1[[i]] <- dict1[[i]]$get_py_object()
                          }
                          
                          if(length(dict1)==1){
                            py$key <- list(as.integer(names(dict1)))
                            py$val <- unname(dict1)
                          } else{
                            py$key <- as.integer(names(dict1))
                            py$val <- unname(dict1)
                          }
                          
                          py_run_string("d = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process19,py_eval("d",convert = F))
                          
                          ans <- py_eval("d")
                          
                          for (a in sequence(length(ans))) {
                            ans[[a]] <- LibCppTest$new()$set_py_object(ans[[a]])
                          }
                          
                          py_run_string("del key;del val;del d")
                          py_gc$collect()
                          
                          tryCatch({
                            eval.parent(substitute(dict<-ans))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                          
                        },
                        
                        
                        #' void  process20(libcpp_map[int, float] & in_)
                        #' 
                        #' takes a named list of double type and names as integer type.
                        process20 = function(dict){
                          
                          # check that list is named, all name values can be converted to integer type and the list elements are of double type.
                          stopifnot(is_list(dict) && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) is_double(d))))
                          
                          # check that list name should be unique.
                          if(!length(unique(names(dict))) == length(names(dict))) {
                            stop("List names should be unique")
                          }
                          
                          if(length(dict)==1){
                            py$key <- list(as.integer(names(dict)))
                            py$val <- unname(dict)
                          } else{
                            py$key <- as.integer(names(dict))
                            py$val <- unname(dict)
                          }
                          
                          py_run_string("d = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process20,py_eval("d",convert = F))
                        
                          ans<-py_eval("d")
                          py_run_string("del key;del val;del d")
                          py_gc$collect()
              
                          tryCatch({
                            eval.parent(substitute(dict<-ans))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                          
                        },
                        
                        
                        #' void  process21(libcpp_map[int, float] & in_, libcpp_map[int,int] & arg2)
                        #' 
                        #' in_ : named list of double type and names as integer type.
                        #' arg2 : named list of integer type and names as integer type.
                        process21 = function(in_,arg2){
                          
                          if(!length(unique(names(in_))) == length(names(in_))) {
                            stop("List in_ contains multiple elements with the same name")
                          }
                          
                          if(!length(unique(names(arg2))) == length(names(arg2))){
                            stop("List arg2 contains multiple elements with the same name")
                          }
                          
                          stopifnot(is_list(in_) && !is.null(names(in_)) && all(check.numeric(names(in_),only.integer = T)) && all(sapply(in_,function(d) is_double(d))))
                          
                          stopifnot(is_list(arg2) && !is.null(names(arg2)) && all(check.numeric(names(arg2),only.integer = T)) && all(sapply(arg2,function(d) is_integer(d))))
                          
                          
                          in_1 <- in_
                          arg2_1 <- arg2
                          
                          if(length(in_1)==1){
                            py$key <- list(as.integer(names(in_1)))
                            py$val <- unname(in_1)
                          } else{
                            py$key <- as.integer(names(in_1))
                            py$val <- unname(in_1)
                          }
                          
                          py_run_string("in_ = dict(zip(key,val))")
                          
                          if(length(arg2_1)==1){
                            py$key <- list(as.integer(names(arg2_1)))
                            py$val <- unname(arg2_1)
                          } else{
                            py$key <- as.integer(names(arg2_1))
                            py$val <- unname(arg2_1)
                          }
                          
                          py_run_string("arg2 = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process21,py_eval("in_",convert = F),py_eval("arg2",convert = F))
                          
                          ans1 <- py_eval("in_")
                          ans2 <- py_eval("arg2")
                          py_run_string("del key;del val;del in_;del arg2")
                          py_gc$collect()
                          
                          tryCatch({
                            eval.parent(substitute(in_<-ans1))
                            eval.parent(substitute(arg2<-ans2))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                          
                        
                        },

                        #' void  process211(libcpp_map[int, float] & in_, libcpp_map[libcpp_string, libcpp_vector[int] ] & arg2)
                        #' 
                        #' in_ : named list of double type and names as integer type.
                        #' arg2 : named list of integer vector and names as character type.
                        process211 = function(in_,arg2){
                          
                          if(!length(unique(names(in_))) == length(names(in_))) {
                            stop("List in_ contains multiple elements with the same name")
                          }
                          
                          if(!length(unique(names(arg2))) == length(names(arg2))){
                            stop("List arg2 contains multiple elements with the same name")
                          }
                          
                          stopifnot(is_list(in_) && !is.null(names(in_)) && all(check.numeric(names(in_),only.integer = T)) && all(sapply(in_,function(d) is_double(d))))
                          
                          stopifnot(is_list(arg2) && !is.null(names(arg2)) && all(sapply(arg2,function(x) is_integer(x))))
                          
                          in_1 <- in_
                          arg2_1 <- arg2
                          
                          #for (i in sequence(length(arg2_1))) {
                          #  if(length(arg2_1[[i]])==1) arg2_1[[i]] <- list(arg2_1[[i]])
                          #}
                          
                          if(length(in_1)==1){
                            py$key <- list(as.integer(names(in_1)))
                            py$val <- unname(in_1)
                          } else{
                            py$key <- as.integer(names(in_1))
                            py$val <- unname(in_1)
                          }
                          
                          py_run_string("in_ = dict(zip(key,val))")
                          
                          if(length(arg2_1)==1){
                            py$key <- list(names(arg2_1))
                            py$val <- unname(arg2_1)
                          } else{
                            py$key <- names(arg2_1)
                            py$val <- unname(arg2_1)
                          }
                          
                          py_run_string("key=[i.encode('utf-8') for i in key];arg2 = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process211,in_ = py_eval("in_",convert = F),arg2 = py_eval("arg2",convert = F))
                          
                          py_run_string("arg2 = dict(zip([i.decode('utf-8') for i in arg2.keys()],arg2.values()))")
                          
<<<<<<< HEAD
                          ans1 <- py_eval("in_")
                          ans2 <- py_eval("arg2")
                          #py_run_string("del key;del val;del in_;del arg2")
                          #py_gc$collect()
                          
                          #tryCatch({
                          #  eval.parent(substitute(in_<-ans1))
                          #  eval.parent(substitute(arg2<-ans2))
                          #  invisible(NULL)
                          #}, error = function(c){invisible(NULL)} )
||||||| merged common ancestors
                          eval.parent(substitute(in_<-py_eval("in_")))
                          eval.parent(substitute(arg2<-py_eval("arg2")))
                          
                          py_run_string("del key;del val;del in_;del arg2")
                          
                          py_gc$collect()
                          
                          invisible()
=======
                          ans1 <- py_eval("in_")
                          ans2 <- py_eval("arg2")
                          #py_run_string("del key;del val;del in_;del arg2")
                          #py_gc$collect()
                          
                          #tryCatch({
                          #  eval.parent(substitute(in_<-ans1))
                          #  eval.parent(substitute(arg2<-ans2))
                          #  invisible(NULL)
                          #}, error = function(c){invisible(NULL)} )
>>>>>>> 9a74d870681a97752d65b344b15df1fa50b250f6
                          
                        },
                        
                        #' void  process212(libcpp_map[int, float] & in_, libcpp_map[libcpp_string, libcpp_vector[ libcpp_vector[int] ] ] & arg2)
                        #' 
                        #' in_ : named list of double type and names as integer type.
                        #' arg2 : named list of (list of integer vectors) and name as character type.
                        process212 = function(in_,arg2){
                          
                          if(!length(unique(names(in_))) == length(names(in_))) {
                            stop("List in_ contains multiple elements with the same name")
                          }
                          
                          if(!length(unique(names(arg2))) == length(names(arg2))){
                            stop("List arg2 contains multiple elements with the same name")
                          }
                          
                          stopifnot(is_list(in_) && !is.null(names(in_)) && all(check.numeric(names(in_),only.integer = T)) && all(sapply(in_,function(d) is_double(d))))
                          
                          stopifnot(is_list(arg2) && !is.null(names(arg2)) && all(sapply(arg2,function(x) is_list(x))))
                          
                          # check for list of vectors of integer type. 
                          for (a in arg2) {
                            for(a_i in a){
                              if(!is.integer(a_i)){
                                stop("arg2 wrong type")
                              }  
                          }
                        }
                          
                          in_1 <- in_
                          arg2_1 <- arg2
                          
                          if(length(in_1)==1){
                            py$key <- list(as.integer(names(in_1)))
                            py$val <- unname(in_1)
                          } else{
                            py$key <- as.integer(names(in_1))
                            py$val <- unname(in_1)
                          }
                          
                          py_run_string("in_ = dict(zip(key,val))")
                          
                          if(length(arg2_1)==1){
                            py$key <- list(names(arg2_1))
                            py$val <- unname(arg2_1)
                          } else{
                            py$key <- names(arg2_1)
                            py$val <- unname(arg2_1)
                          }
                          
                          
                          py_run_string("key=[i.encode('utf-8') for i in key];arg2 = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process212,py_eval("in_",convert = F),py_eval("arg2",convert = F))
                          
                          py_run_string("arg2 = dict(zip([i.decode('utf-8') for i in arg2.keys()],arg2.values()))")
                          
                          ans1 <- py_eval("in_")
                          ans2 <- py_eval("arg2")
                          py_run_string("del key;del val;del in_;del arg2")
                          py_gc$collect()
                          
                          tryCatch({
                            eval.parent(substitute(in_<-ans1))
                            eval.parent(substitute(arg2<-ans2))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                          
                        },
                        
                        
                        #' void  process213(libcpp_map[int, float] & in_, libcpp_map[libcpp_string, libcpp_vector[ libcpp_vector[Int] ] ] & arg2)
                        #' 
                        #' in_ : named list of double type and names as integer type.
                        #' arg2 : named list of (list of Int objects) and name as character type.
                        process213 = function(in_,arg2){
                          
                          if(!length(unique(names(in_))) == length(names(in_))) {
                            stop("List in_ contains multiple elements with the same name")
                          }
                          
                          if(!length(unique(names(arg2))) == length(names(arg2))){
                            stop("List arg2 contains multiple elements with the same name")
                          }
                          
                          stopifnot(class(in_)=="list" && !is.null(names(in_)) && all(check.numeric(names(in_),only.integer = T)) && all(sapply(in_,function(d) typeof(d)=="double")))
                          
                          stopifnot(class(arg2)=="list" && !is.null(names(arg2)) && all(sapply(arg2,function(x) class(x)=="list")))
                          
                          in_1 <- in_
                          arg2_1 <- arg2
                          
                          # check for list of "Int" objects. 
                          for (a in sequence(length(arg2_1))) {
                            
                            #stopifnot(class(arg2_1[[a]])=="list")
                            
                            for(b in sequence(length(arg2_1[[a]]))){
                              if(!all(class(arg2_1[[a]][[b]])==c("Int","R6"))){
                                stop("arg2 wrong type")
                              }
                              arg2_1[[a]][[b]] <- arg2_1[[a]][[b]]$get_py_object()
                              
                            }
                          
                          }
                          
                          if(length(in_1)==1){
                            py$key <- list(as.integer(names(in_1)))
                            py$val <- unname(in_1)
                          } else{
                            py$key <- as.integer(names(in_1))
                            py$val <- unname(in_1)
                          }
                          
                          py_run_string("in_ = dict(zip(key,val))")
                          
                          if(length(arg2_1)==1){
                            py$key <- list(names(arg2_1))
                            py$val <- unname(arg2_1)
                          } else{
                            py$key <- names(arg2_1)
                            py$val <- unname(arg2_1)
                          }
                          
                          py_run_string("key=[i.encode('utf-8') for i in key];arg2 = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process213,py_eval("in_",convert = F),py_eval("arg2",convert = F))
                          
                          py_run_string("arg2 = dict(zip([i.decode('utf-8') for i in arg2.keys()],arg2.values()))")
                          
                          arg2_1 <- py_eval("arg2")
                          
                          # converting python object to R.
                          for (a in sequence(length(arg2_1))) {
                            for(b in sequence(length(arg2_1[[a]]))){
                             
                              arg2_1[[a]][[b]] <- LibCppTest$new()$set_py_object(arg2_1[[a]][[b]])
                              
                            }
                            
                          }
                          
                          ans1 <- py_eval("in_")
                          ans2 <- py_eval("arg2")
                          py_run_string("del key;del val;del in_;del arg2")
                          py_gc$collect()
                          
                          tryCatch({
                            eval.parent(substitute(in_<-ans1))
                            eval.parent(substitute(arg2<-ans2))
                          }, error = function(c){} )
                        },
                        
                        #' void  process214(libcpp_map[int, float] & in_, libcpp_map[libcpp_string, libcpp_vector[ libcpp_pair[int, int] ] ] & arg2)
                        #' 
                        #' in_ : named list of double type and names as integer type.
                        #' arg2 : named list of (list of integer vector of length 2) and names as character type.
                        process214 = function(in_,arg2){
                          
                          if(!length(unique(names(in_))) == length(names(in_))) {
                            stop("List in_ contains multiple elements with the same name")
                          }
                          
                          if(!length(unique(names(arg2))) == length(names(arg2))){
                            stop("List arg2 contains multiple elements with the same name")
                          }
                          
                          stopifnot(class(in_)=="list" && !is.null(names(in_)) && all(check.numeric(names(in_),only.integer = T)) && all(sapply(in_,function(d) typeof(d)=="double")))
                          
                          stopifnot(class(arg2)=="list" && !is.null(names(arg2)) && all(sapply(arg2,function(x) class(x)=="list")))
                          
                          in_1 <- in_
                          arg2_1 <- arg2
                          
                          # check for list of 2 integers. 
                          for (a in sequence(length(arg2_1))) {
                            
                            for(b in sequence(length(arg2_1[[a]])))
                            {
                              if(!( length(arg2_1[[a]][[b]])==2 && all(sapply(arg2_1[[a]][[b]],function(x) is_integer(x))) ))
                              {
                                stop("arg2 wrong type")
                              }
                            }
                            
                          }
                          
                          
                          if(length(in_1)==1){
                            py$key <- list(as.integer(names(in_1)))
                            py$val <- unname(in_1)
                          } else{
                            py$key <- as.integer(names(in_1))
                            py$val <- unname(in_1)
                          }
                          
                          py_run_string("in_ = dict(zip(key,val))")
                          
                          if(length(arg2_1)==1){
                            py$key <- list(names(arg2_1))
                            py$val <- unname(arg2_1)
                          } else{
                            py$key <- names(arg2_1)
                            py$val <- unname(arg2_1)
                          }
                          
                          py_run_string("key=[i.encode('utf-8') for i in key];arg2 = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process214,py_eval("in_",convert = F),py_eval("arg2",convert = F))
                          
                          py_run_string("arg2 = dict(zip([i.decode('utf-8') for i in arg2.keys()],arg2.values()))")
                          
                          ans1 <- py_eval("in_")
                          ans2 <- py_eval("arg2")
                          py_run_string("del key;del val;del in_;del arg2")
                          py_gc$collect()
                          
                          tryCatch({
                            eval.parent(substitute(in_<-ans1))
                            eval.parent(substitute(arg2<-ans2))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                        },
                        
                        #' void  process22(libcpp_set[int] &, libcpp_set[float] &)
                        #' 
                        #' takes two lists one of integer type and other of double type.
                        process22 = function(in_0,in_1){
                          stopifnot(is_list(in_0) && all(sapply(in_0, function(x) is_integer(x))) && !(TRUE %in% duplicated(in_0)))
                          stopifnot(is_list(in_1) && all(sapply(in_1, function(x) is_double(x))) && !(TRUE %in% duplicated(in_1)))
                          
                          py$in_0 <- in_0
                          py$in_1 <- in_1
                          
                          py_run_string("in_0 = set(in_0);in_1 = set(in_1)")
                          
                          py_call(private$py_obj$process22,py_eval("in_0",convert = F),py_eval("in_1",convert = F))
                          
                          ans1 <- py_eval("list(in_0)")
                          ans2 <- py_eval("list(in_1)")
                          py_run_string("del in_0;del in_1")
                          py_gc$collect()
                          
                          tryCatch({
                            eval.parent(substitute(in_0<-as.list(ans1)))
                            eval.parent(substitute(in_1<-as.list(ans2)))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                          
                        },
                        
                        
                        #' void  process23(libcpp_vector[int] &, libcpp_vector[float] &)
                        #' 
                        #' takes two lists of type integer and double.
                        process23 = function(in_0,in_1){
                          
                          stopifnot(is_list(in_0) && all(sapply(in_0, function(x) is_integer(x))) )
                          stopifnot(is_list(in_1) && all(sapply(in_1, function(x) is_double(x))) )
                          
                          a1 <- r_to_py(in_0)
                          a2 <- r_to_py(in_1)
                          private$py_obj$process23(a1,a2)
                          
                          tryCatch({
                            eval.parent(substitute(in_0<-as.list(py_to_r(a1))))
                            eval.parent(substitute(in_1<-as.list(py_to_r(a2))))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                          
                        },
                        
                        
                        #' void  process24(libcpp_pair[int, float] & in_, libcpp_pair[int,int] & arg2)
                        #' 
                        #' in_ : a list of integer and double type.
                        #' arg2 : a list of integer type.
                        process24 = function(in_,arg2){
                          
                          stopifnot(is_list(in_) && length(in_)==2 && is_scalar_integer(in_[[1]]) && is_scalar_double(in_[[2]]))
                          stopifnot(is_list(arg2) && length(arg2)==2 && is_scalar_integer(arg2[[1]]) && is_scalar_integer(arg2[[2]]))
                          
                          a1 <- r_to_py(in_)
                          a2 <- r_to_py(arg2)
                          private$py_obj$process24(a1,a2)
                          
                          tryCatch({
                            eval.parent(substitute(in_<-py_to_r(a1)))
                            eval.parent(substitute(arg2<-as.list(py_to_r(a2))))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                          
                        },
                        
                        #' int process25(libcpp_vector[Int] in_)
                        #' 
                        #' in_ : a list of Int objects.
                        process25 = function(in_){
                          stopifnot(inherits(in_,"list"))
                          
                          # converting to python object.
                          for (i in sequence(length(in_))) {
                            
                            if(!all(class(in_[[i]])==c("Int","R6"))){
                              stop("arg in_ wrong type")
                            }
                            in_[[i]] <- in_[[i]]$get_py_object()
                          }
                          
                          return(private$py_obj$process25(in_))
                        },
                        
                        #' int process26(libcpp_vector[libcpp_vector[Int]] in_)
                        #' 
                        #' in_ : a list of (list of Int objects)
                        process26 = function(in_){
                          stopifnot(class(in_) == "list")
                          
                          # checks if nested list of "Int" objects are being called.
                          # conversion to python object.
                          for (i in sequence(length(in_))) {
                            for(j in sequence(length(in_[[i]]))){
                              
                              stopifnot(class(in_[[i]][[j]]) == c("Int","R6"))
                              in_[[i]][[j]] <- in_[[i]][[j]]$get_py_object()
                            }
                          }
                          
                          return(private$py_obj$process26(in_))
                        },
                        
                        #' int process27(libcpp_vector[libcpp_vector[libcpp_vector[Int]]] in_)
                        #' 
                        #' in_ : a list of (list of (list of Int objects))
                        process27 = function(in_){
                          stopifnot(class(in_) == "list")
                          
                          for (i in sequence(length(in_))) {
                            for (j in sequence(length(in_[[i]]))) {
                              for (k in sequence(length(in_[[i]][[j]]))) {
                                
                                stopifnot(class(in_[[i]][[j]][[k]]) == c("Int","R6"))
                                in_[[i]][[j]][[k]] <- in_[[i]][[j]][[k]]$get_py_object()
                              
                                }
                              }
                            }
                          
                          return(private$py_obj$process27(in_))
                          
                        },
                        
                        #' int process28(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] in_)
                        #' 
                        process28 = function(in_){
                          stopifnot(class(in_) == "list")
                          
                          for (i in sequence(length(in_))) {
                            for (j in sequence(length(in_[[i]]))) {
                              for (k in sequence(length(in_[[i]][[j]]))) {
                                for (l in sequence(length(in_[[i]][[j]][[k]]))) {
                                 
                                  stopifnot(class(in_[[i]][[j]][[k]][[l]]) == c("Int","R6"))
                                  in_[[i]][[j]][[k]][[l]] <- in_[[i]][[j]][[k]][[l]]$get_py_object()
                                    
                                }
                              }
                            }
                          }
                          
                          return(private$py_obj$process28(in_))
                        },
                        
                        #' void  process29(libcpp_vector[libcpp_vector[Int]] & in_)
                        #' 
                        process29 = function(in_){
                          stopifnot(class(in_) == "list")
                          in_1 <- in_
                          
                          for (i in sequence(length(in_1))) {
                            for(j in sequence(length(in_1[[i]]))){
                              stopifnot(class(in_1[[i]][[j]]) == c("Int","R6"))
                              in_1[[i]][[j]] <- in_1[[i]][[j]]$get_py_object()
                            }
                          }
                          
                          in_1 <- r_to_py(in_1)
                          private$py_obj$process29(in_1)
                          in_1 <- py_to_r(in_1)
                          
                          for (i in sequence(length(in_1))) {
                            for (j in sequence(length(in_1[[i]]))) {
                              in_1[[i]][[j]] <- Int$new(in_1[[i]][[j]]$i_)
                            }
                          }
                          
                          tryCatch({
                            eval.parent(substitute(in_<-in_1))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                        },
                        
                        #' void  process30(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] & in_)
                        #' 
                        process30 = function(in_){
                          stopifnot(class(in_) == "list")
                          in_1 <- in_
                          
                          for (i in sequence(length(in_1))) {
                            for (j in sequence(length(in_1[[i]]))) {
                              for (k in sequence(length(in_1[[i]][[j]]))) {
                                for (l in sequence(length(in_1[[i]][[j]][[k]]))) {
                                  
                                  stopifnot(class(in_1[[i]][[j]][[k]][[l]]) == c("Int","R6"))
                                  in_1[[i]][[j]][[k]][[l]] <- in_[[i]][[j]][[k]][[l]]$get_py_object()
                                  
                                }
                              }
                            }
                          }
                          
                          in_1 <- r_to_py(in_1)
                          private$py_obj$process30(in_1)
                          in_1 <- py_to_r(in_1)
                          
                          for (i in sequence(length(in_1))) {
                            for (j in sequence(length(in_1[[i]]))) {
                              for (k in sequence(length(in_1[[i]][[j]]))) {
                                for (l in sequence(length(in_1[[i]][[j]][[k]]))) {
                                  
                                  in_1[[i]][[j]][[k]][[l]] <- Int$new(in_1[[i]][[j]][[k]][[l]]$i_)
                                  
                                }
                              }
                            }
                          }
                          
                          tryCatch({
                            eval.parent(substitute(in_<-in_1))
                            invisible(NULL)
                          }, error = function(c){invisible(NULL)} )
                        },
                        
                        #' int process31(libcpp_vector[int] in_)
                        process31 = function(in_){
                          
                          stopifnot(is_list(in_) && all(sapply(in_, function(x) is_integer(x))) )
                          return(private$py_obj$process31(in_))
                        },
                        
                        #' int process32(libcpp_vector[libcpp_vector[int]] in_)
                        process32 = function(in_){
                          stopifnot(inherits(in_,"list"))
                          
                          stopifnot(all(sapply(in_, function(x) is_integer(x))))
                          
                          return(private$py_obj$process32(in_))
                        },
                        
                        #' int   process33(shared_ptr[Int] in_)
                        process33 = function(in_){
                          stopifnot(all(class(in_)==c("Int","R6")))
                          return(private$py_obj$process33(in_$get_py_object()))
                        },
                        
                        #' shared_ptr[Int] process34(shared_ptr[Int] in_)
                        process34 = function(in_){
                          stopifnot(all(class(in_)==c("Int","R6")))
                          in_$.__enclos_env__$private$py_obj <- private$py_obj$process34(in_$get_py_object())
                          return(in_)
                        },
                        
                        #' shared_ptr[const Int] process35(shared_ptr[Int] in_)
                        process35 = function(in_){
                          stopifnot(all(class(in_)==c("Int","R6")))
                          return(Int$new(private$py_obj$process35(in_$get_py_object())$i_))
                        },
                        
                        #' int  process36(Int* in_)
                        process36 = function(in_){
                          stopifnot(all(class(in_)==c("Int","R6")))
                          return(private$py_obj$process36(in_$get_py_object()))
                        },
                        
                        #' Int*   process37(Int* in_)
                        process37 = function(in_){
                          stopifnot(all(class(in_)==c("Int","R6")))
                          if(in_$i_ == 18){
                            return(private$py_obj$process37(in_$get_py_object()))
                          }
                          return(Int$new(private$py_obj$process37(in_$get_py_object())$i_))
                        },
                        
                        #' libcpp_vector[libcpp_vector[UInt]] process38(int)
                        process38 = function(in_0){
                          stopifnot(is_scalar_integer(in_0))
                          return(private$py_obj$process38(in_0))
                        },
                        
                        #' const Int* process39(Int* in_)
                        process39 = function(in_){
                          stopifnot(all(class(in_)==c("Int","R6")))
                          return(Int$new(private$py_obj$process39(in_$get_py_object())$i_))
                        },

                        #' int process40(AbstractBaseClass* in_)
                        process40 = function(in_){
                          stopifnot( all(class(in_)==c("ABS_Impl1","R6")) || all(class(in_)==c("ABS_Impl2","R6")) )
                          
                          switch(class(in_)[1],"ABS_Impl1" = return(private$py_obj$process40(in_$get_py_object())),"ABS_Impl2" = return(private$py_obj$process40(in_$get_py_object())))
                        },

                        get = function(){
                          return(private$py_obj$gett())
                        },
                        
                        get_py_object = function(){
                          return(private$py_obj)
                        },
                        
                        set_py_object = function(ob){
                          private$py_obj <- ob
                          return(self)
                        }
                        
                    )
                    
)

# locking all the classes, methods are locked by default.
ABS_Impl1$lock()
ABS_Impl2$lock()
Int$lock()
LibCppTest$lock()
