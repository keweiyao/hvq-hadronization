c**********************************************************************c
c                                                                      c
      subroutine oscfileheaderio
c                                                                      c
c     input and output of the OSCAR 1997A file header.                 c
c                                                                      c
c     last change: 08/06/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      
c**** read file-header ************************************************c 
      
      read (unit=inputline,fmt=901,err=301,end=301) fh_format
      read (unit=INU,fmt=901,err=301,end=301) fh_content
      read (unit=INU,fmt=903,err=301,end=301)
     .     fh_modelname, fh_version, fh_tarmass, fh_tarcharge,
     .     fh_promass, fh_procharge, fh_refframe, fh_eibeam, 
     .     fh_parpernuc
   
c**** write file-header ***********************************************c

      write (unit=OUTU,fmt=901,err=302) fh_format
      write (unit=OUTU,fmt=901,err=302) fh_content
      write (unit=OUTU,fmt=902,err=302)
     .      fh_modelname, fh_version, fh_tarmass, fh_tarcharge,
     .      fh_promass, fh_procharge, fh_refframe, fh_eibeam,
     .      fh_parpernuc 

      return

c**** errors **********************************************************c

 301  write (ERRU,*) 'ERROR while reading file-header'
      stop
 302  write (ERRU,*) 'ERROR while writing file-header'
      stop

      end

