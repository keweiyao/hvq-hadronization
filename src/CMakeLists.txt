add_executable(${PROJECT_NAME}
  analysis.f      
  copypartoscar.f 
  fragdecay.f  
  isheavymeson.f  
  oscfileheaderio.f  
  pythia-6.4.21.f  
  readeventoscar.f  
  writeevent13.f
  checkformat.f   
  endprog.f        
  isparton.f      
  parameter_inp.f    
  rand.f           
  recomb.f          
  writeeventoscar.f
  copypart13.f  
  filter.f         
  initpythia.f  
  ityp2pdg.f      
  pdg2id.f           
  readevent13.f    
  rotbos.f
  )

# install compiled binary
install (TARGETS ${PROJECT_NAME} DESTINATION ${PROJECT_NAME})
