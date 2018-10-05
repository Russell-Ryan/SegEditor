function segeditor_readhdr,file,hdr
  ok=~strcmp(file,'') && file_test(file)
  if ok then begin
     hdr=headfits(file,ext=0,/silent)
     ok=size(hdr,/type) eq 7
  endif
  return,ok
end
  
pro segeditor,seg,img,HELP=help,_EXTRA=_extra
  if keyword_set(HELP) then begin
     print,'segeditor,seg,img,[/help,_EXTRA=_extra]'
     print,'     seg - full path to segmentation map'
     print,'     img - full path to direct image'
     return
  endif


  filter=[['*fit;*fits','*fit.gz;*fits.gz'],$
          ['fits','zipped fits']]

  if size(seg,/type) ne 7 then begin
     seg=dialog_pickfile(/read,/must_exist,filter=filter,$
                         title='Select Segmentation Image')
  endif
  if ~segeditor_readhdr(seg,hseg) then return


  if size(img,/type) ne 7 then begin
     img=dialog_pickfile(/read,/must_exist,filter=filter,$
                         title='Select Direct Image')
  endif
  if ~segeditor_readhdr(img,himg) then return
  
  naxiss=sxpar(hseg,'NAXIS*')
  naxisi=sxpar(himg,'NAXIS*')
  if ~array_equal(naxisi,naxiss) then return
  
  obj=obj_new('segeditor',seg,img,_EXTRA=_extra)

end

