myTry <- function(expr) {
  withRestarts(
               withCallingHandlers(expr,
                                   error = function(e) {
                                     error  = e
                                     calls  = sys.calls()
                                     frames = sys.frames()
                                     invokeRestart("myAbort", e, calls, frames)
                                   }),
               myAbort = function(e, calls, frames){
                 err = list(error = e, calls=calls, frames=frames)
                 class(err)<-c("try-error")
                 return(err)
               }
               )
}

myTraceback <- function(trace, optFunctions2Skip=c() ) {
  functions2Skip = c('withRestarts','withCallingHandlers','invokeRestart', 'withOneRestart' ,'myTry', 'doWithOneRestart', '.handleSimpleError', optFunctions2Skip)
  for( i in 1:length(trace$frames)){
    env  = trace$frames[[ i ]]
    func = trace$calls [[ i ]]
    funcStr  = as.character(trace$calls[[i]])
    
    funcNameStr = funcStr[1]

    args=FALSE
    simple.Error=FALSE
    
    if(length(funcStr) > 1){
      funcArgs    = funcStr[2:length(funcStr)]
      args = TRUE
      if(any(grep('simpleError',funcArgs))){simple.Error=TRUE}
    }
    if ( !(funcNameStr %in% functions2Skip) && !simple.Error){
      cat('\n---------------------------------\n')
      if(args) cat(funcNameStr,'(',funcArgs,')','\n')
      else     cat(funcNameStr,'()','\n')
      cat('- - - - - - - - - - - - - - - - -\n')
      
      vars=ls(env)
      for(v in vars){
        cat(v,'=')
        print(get(v, envir=env))
      }
    }
  }
}
