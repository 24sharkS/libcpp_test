library(reticulate)
library(varhandle)
library(R6)

libcpp <- reticulate::import("py_libcpp_test",convert = T)
py_gc <- reticulate::import("gc")


Int <- R6Class(classname = "Int",cloneable = FALSE,
               private = list(
                  .py_obj = NA
               ),
               
               public = list(
                 initialize = function(i){
                   stopifnot(class(i) == c("Int","R6") || class(i) == "integer" && length(i)==1)
                   if(is.R6(i) && class(i)[1] == "Int"){
                     private$.py_obj <- libcpp$Int(i$get_py_object())
                   } else{
                     private$.py_obj <- libcpp$Int(i)
                   }
                 },
                 
                 get_py_object = function(){
                   return(private$.py_obj)
                 }
               ),
               
               active = list(
                 i_ = function(value){
                   if(missing(value)){
                     return(private$.py_obj$i_)
                   } else {
                     private$.py_obj <- libcpp$Int(value)
                   }
                 }
               )
)

LibCppTest <- R6Class(classname = "LibCppTest",cloneable = FALSE,
                      private = list(
                        py_obj = NA
                      ),
                      
                      public = list(
                        initialize = function(...){
                          arg_list <- list(...)
                          #print(class(arg_list[1]))
                          if(length(arg_list)==0){
                            private$py_obj <- libcpp$LibCppTest()
                          } else if(length(arg_list)==1){
                            if(class(arg_list[[1]])=="integer"){
                              private$py_obj <- libcpp$LibCppTest(arg_list[[1]])
                            }
                            else{
                              stop("wrong argument!!")
                            }
                          }
                          else{
                            stop("wrong argument!!")
                          }
                        },
                        
                        twist = function(pair){
                          stopifnot(length(pair)==2 && class(pair[[1]])=="character" && class(pair[[2]])=="integer")
                          return(private$py_obj$twist(pair))
                        },
                        
                        process = function(vec){
                          stopifnot(class(vec)=="integer")
                          return(private$py_obj$process(vec))
                        },
                        
                        process2 = function(vec){
                          stopifnot(length(vec)==2 && class(vec)=="integer")
                          return(private$py_obj$process2(vec))
                        },
                        
                        
                        
                        
                        process3 = function(vec){
                          stopifnot(length(vec)==2 && class(vec[[2]])=="integer" && class(vec[[1]])=="LibCppTest")
                          vec[[1]] <- vec[[1]]$get_py_object()
                          vec2 <- private$py_obj$process3(vec)
                          vec2[[1]] <- LibCppTest$new(vec2[[1]]$gett())
                          return(vec2)
                        },
                        
                        
                        
                        
                        process4 = function(pair){
                          pair[[2]] <- pair[[2]]$get_py_object()
                          
                          pair2 <- private$py_obj$process4(pair)
                          
                          pair2[[2]] <- LibCppTest$new(pair2[[2]]$gett())
                          return(pair2)
                        },
                        
                        process5 = function(pair){
                          pair[[1]] <- pair[[1]]$get_py_object()
                          pair[[2]] <- pair[[2]]$get_py_object()
                          
                          pair2 <- private$py_obj$process5(pair)
                          
                          pair2[[1]] <- LibCppTest$new(pair2[[1]]$gett())
                          pair2[[2]] <- LibCppTest$new(pair2[[2]]$gett())
                          return(pair2)
                        },
                        
                        process6 = function(vec_pair){
                          stopifnot(class(vec_pair)=="list")
                          return(private$py_obj$process6(vec_pair))
                        },
                        
                        process7 = function(pair){
                          stopifnot(length(pair)==2 && pair[1] %in% as.integer(c(0,1)) && class(pair[2])=="integer")
                          
                          return(private$py_obj$process7(pair))
                        },
                        
                        process8 = function(vec_pair){
                          stopifnot(class(vec_pair)=="integer")
                          for (v in vec_pair) {
                            stopifnot(v %in% as.integer(c(0,1)))
                          }
                          
                          return(private$py_obj$process8(vec_pair))
                        },
                        
                        
                        
                        process9 = function(set_int){
                          
                          stopifnot(class(set_int)=="list")
                          
                          # create python list corresponding to R list.
                          py$o1 <- set_int
                          
                          # convert the python list to set.
                          py_run_string("o1 = set(o1)")
                          
                          # call the python function `process9` passing the set as argument.
                          # function returns a set also.
                          py$o1 <- py_call(private$py_obj$process9,py$o1)
                          
                          ans <- py_eval("list(o1)")
                          
                          # Deleting the python object.
                          py_run_string("del o1")
                          py_gc$collect()
                          
                          return(ans)
                        },
                        
                        
                        process11 = function(input){
                          
                          stopifnot(class(input)=="list")
                          for (i in sequence(length(input))){
                            stopifnot(is.R6(input[[i]]) && class(input[[i]])[1] == "LibCppTest")
                            input[[i]] <- input[[i]]$get_py_object()
                          }
                          
                          py$o1 <- input
                          
                          py_run_string("o1 = set(o1)")
                          
                          py$o1 <- py_call(private$py_obj$process11,py$o1)
                          
                          ans <- py_eval("list(o1)")
                          
                          py_run_string("del o1")
                          py_gc$collect()
                          
                          ans <- lapply(ans,function(x) LibCppTest$new(x$gett()))
                          
                          return(ans)
                        },
                        
                        # process 12
                        process12 = function(a,b){
                          stopifnot(typeof(a)=="integer" && typeof(b)=="double" && length(a)==1 && length(b)==1)
                          return(private$py_obj$process12(a,b))
                        },
                        
                        # process 13
                        process13 = function(enum,int){
                          stopifnot(class(enum)=="integer" && class(int)=="integer" && enum %in% c(1,0) && length(int)==1)
                          return(private$py_obj$process13(enum,int))
                        },
                        
                        # process 14
                        process14 = function(enum,int){
                          stopifnot(class(enum)=="integer" && class(int)=="integer" && enum %in% c(1,0) && length(int)==1)
                          return(private$py_obj$process13(enum,int))
                        },
                        
                        # process 15
                        process15 = function(int){
                          stopifnot(class(int)=="integer" && length(int)==1)
                          
                          ans <- private$py_obj$process15(int)
                          ans[[1]] <- LibCppTest$new()$set_py_object(ans[[1]])
                          return(ans)
                        },
                        
                        # process 16
                        process16 = function(dict){
                          stopifnot(class(dict)=="list" && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) typeof(d)=="double")))
                          
                          if(length(dict)==1){
                            py$key <- list(as.integer(names(dict)))
                            py$val <- list(unlist(dict,use.names = F))
                          } else{
                            py$key <- as.integer(names(dict))
                            py$val <- unlist(dict,use.names = F)
                          }
                          
                          ans <- py_to_r(py_call(private$py_obj$process16,py_eval("dict(zip(key,val))",convert = F))) 
                          
                          py_run_string("del key;del val")
                          
                          py_gc$collect()
                          
                          return(ans)
                        },
                        
                        # process 17
                        process17 = function(dict){
                          
                          # R allows for different elements with same name.
                          if(!length(unique(names(dict))) == length(names(dict))) {
                            stop("List contains multiple elements with the same name")
                          }
                          
                          stopifnot(class(dict)=="list" && !is.null(names(dict)) && all(sapply(names(dict),function(d) as.integer(d) %in% c(1L,0L))) && all(sapply(dict,function(d) typeof(d)=="double")))
                          
                          if(length(dict)==1){
                            py$key <- list(as.integer(names(dict)))
                            py$val <- list(unlist(dict,use.names = F))
                          } else{
                            py$key <- as.integer(names(dict))
                            py$val <- unlist(dict,use.names = F)
                          }
                          
                          ans <- py_to_r(py_call(private$py_obj$process17,py_eval("dict(zip(key,val))",convert = F)))
                          
                          py_run_string("del key;del val")
                          
                          py_gc$collect()
                          
                          return(ans)
                          
                        },
                        
                        # process 18
                        process18 = function(dict){
                          
                          stopifnot(class(dict)=="list" && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) all(class(d)==c("LibCppTest","R6")))))
                          
                          for (i in sequence(length(dict))) {
                            dict[[i]] <- dict[[i]]$get_py_object()
                          }
                          
                          if(length(dict)==1){
                            py$key <- list(as.integer(names(dict)))
                            py$val <- list(unlist(dict,use.names = F))
                          } else{
                            py$key <- as.integer(names(dict))
                            py$val <- unlist(dict,use.names = F)
                          }
                          
                          ans <- py_to_r(py_call(private$py_obj$process18,py_eval("dict(zip(key,val))",convert = F)))
                          
                          py_run_string("del key;del val")
                          
                          py_gc$collect()
                          
                          return(ans)
                          
                        },
                        
                        # process19
                        process19 = function(dict){
                          stopifnot(class(dict)=="list" && !is.null(names(dict)) && all(check.numeric(names(dict),only.integer = T)) && all(sapply(dict, function(d) class(d)==c("LibCppTest","R6"))))
                          
                          dict1 <- dict
                          
                          for (i in sequence(length(dict))) {
                            dict1[[i]] <- dict1[[i]]$get_py_object()
                          }
                          
                          if(length(dict1)==1){
                            py$key <- list(as.integer(names(dict1)))
                            py$val <- list(unlist(dict1,use.names = F))
                          } else{
                            py$key <- as.integer(names(dict1))
                            py$val <- unlist(dict1,use.names = F)
                          }
                          
                          py_run_string("d = dict(zip(key,val))")
                          
                          py_call(private$py_obj$process19,py_eval("d",convert = F))
                          
                          ans <- py_eval("d")
                          
                          for (a in sequence(length(ans))) {
                            ans[[a]] <- LibCppTest$new()$set_py_object(ans[[a]])
                          }
                          
                          py_run_string("del key;del val;del d")
                          
                          py_gc$collect()
                          
                          eval.parent(substitute(dict<-ans))
                        },
                        
                        
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
                          
                          ans <- private$py_obj$process26(in_)
                          
                          return(ans)
                        },
                        
                        get = function(){
                          return(private$py_obj$gett())
                        },
                        
                        get_py_object = function(){
                          return(private$py_obj)
                        },
                        
                        set_py_object = function(x){
                          private$py_obj <- x
                          invisible(self)
                        }
                    )
                    
)
