pro segeditor_event,event
  widget_control,event.top,get_uval=self
  uname=widget_info(event.id,/uname)
  call_method,uname,self,event
end
pro segeditor_leftmode_event,event
  widget_control,event.top,get_uval=self
  self->SetMode,widget_info(event.id,/uname)
end

pro segeditor_draw_event,event
  widget_control,event.id,get_uval=self
  self->DrawEvent,event
end


pro segeditor_cleanup,base,FORCE=force
  if widget_info(base,/valid) then begin
     
     widget_control,base,get_uval=self
     if obj_valid(self) then begin
        if ~self->saved() && ~keyword_set(FORCE) then begin
           ans=dialog_message('Quit without saving?',/center,/question,$
                              dialog_parent=base,title='Quit Segeditor')
           quit=strcmp(ans,'Yes',/fold)
        endif else quit=1b
        
        if quit then begin
           widget_control,base,map=0b
           obj_destroy,self
           widget_control,base,/destroy
        endif
     endif
  endif
end

pro segeditor::Resize,newsize
  print,'Not fully implemented.  must adjust the text positions'
;  delta=newsize-self.winsize
;  q=widget_info(self.winst,/geom)
;  widget_control,self.wdraw,xsize=newsize,ysize=newsize
;  widget_control,self.winst,xsize=q.xsize+delta
;  self.oWindow->Draw,self.oView
;  self.winsize=newsize
end
  
pro segeditor::properties,event
  widget_control,event.id,get_uval=obj
  property,obj,group=event.top
end
function segeditor::saved
  return,self.saved
end

pro segeditor::GetSegMap,seg
  seg=self.seg->AllImage()
end
pro segeditor::Operations,event
  if ~obj_valid(self.operations) then begin
     self.operations=obj_new('operations',self)
  endif else begin
     self.operations->show
  endelse     
end


pro segeditor::Compress,event
  widget_control,event.top,hourglass=1b
  
  ;do the undo
  old=self.seg->AllImage()

  u=uniq(old,sort(old,/l64))
  new=value_locate(old[temporary(u)],old,/l64)

  gpix=where(old ne 0,npix)

  dim=size(old,/dim)


  xy=array_indices(dim,gpix,/dim)
  undo={x:reform(xy[0,*],npix),y:reform(xy[1,*],npix),$
        tile:[0L,0,dim[0]-1,dim[1]-1],old:old[gpix],new:new[gpix]}
  
  self->SetUndo,undo
  self->SetValues,undo.x,undo.y,undo.tile,undo.new ;set data to VIEW

  widget_control,event.top,hourglass=0b
end


pro segeditor::new,event
  self->DeleteRegion,event,/all
end

pro segeditor::SaveAs,event,FILENAME=filename
  if ~keyword_set(FILENAME) then $
     filename=dialog_pickfile(dialog_parent=event.top,file=self.file,$
                              filter='*_seg.fits',/write,$
                              title='write segmentation map')
  if ~strcmp(filename,'',/fold) then begin
     self.file=filename
     self->Save,event
  endif
end
pro segeditor::Save,event
  if self.updateheader then begin
     self.fitsfile->sxaddhist,'File created by SegEditor: Ryan (2018)'
     self.updateheader=0b
  endif
  
  self.seg->Write,self.file
  self.saved=1b
end

pro segeditor::tabulate,event
  if ~obj_valid(self.region_stats) then begin
     self.region_stats=obj_new('region_stats',self.seg->AllImage(),self,$
                               group_leader=event.top)  
  endif else begin
     self.region_stats->SetImage,self.seg->AllImage()
     self.region_stats->show
  endelse     
end
pro segeditor::help,event
  psep=path_sep()
  book=strjoin([self.install,'..','etc','help','segeditor.html'],psep)
  online_help,book=book  
end

pro segeditor::edit,event
  widget_control,event.id,get_uval=obj
  property,obj,group=event.top,title='Edit Property'
end


pro segeditor::SetImage,TYPE=type
  if keyword_set(TYPE) then self.scale=type

  case self.scale of
     'log': begin
        m=self.minmax[0]-(self.minmax[1]-self.minmax[0])*0.01
        scl=bytscl(alog10((*self.subimg)-m),$
                   alog10(self.minmax[0]-m),$
                   alog10(self.minmax[1]-m),$
                   top=self.top,/nan)
     end
     'linear': scl=bytscl(*self.subimg,self.minmax[0],self.minmax[1],$
                          top=self.top)
     'sqrt': begin
        m=self.minmax[0]-(self.minmax[1]-self.minmax[0])*0.01
        scl=bytscl(sqrt((*self.subimg)-m),$
                   sqrt(self.minmax[0]-m),$
                   sqrt(self.minmax[1]-m),$
                   top=self.top,/nan)
     end
     'square': begin
        m=self.minmax[0]-(self.minmax[1]-self.minmax[0])*0.01
        scl=bytscl(((*self.subimg)-m)^2,$
                   (self.minmax[0]-m)^2,$
                   (self.minmax[1]-m)^2,$
                   top=self.top,/nan)
     end
     'power law': begin
        m=self.minmax[0]-(self.minmax[1]-self.minmax[0])*0.01
        scl=bytscl(self.powscl^((*self.subimg)-m),$
                   self.powscl^(self.minmax[0]-m),$
                   self.powscl^(self.minmax[1]-m),$
                   top=self.top,/nan)
     end
     'histo equal': begin
        scl=bytscl(hist_equal(*self.subimg,minv=self.minmax[0],$
                              maxv=self.minmax[1]),/nan,top=self.top)
     end
     

     
     else: stop,'Unknown Scale function'
  endcase

  self.oImg->SetProperty,data=temporary(scl)

end

pro segeditor::ChangePalette,event
  x=event.x/self.winsize
  y=event.y/self.winsize
  case self.rightmode of
     'ag': begin
        self.alpha=0>y<1.
;        gamma=0.1>(10.*x)<10.
;        self.oPalette->SetProperty,gamma=gamma
        self.oSeg->SetProperty,alpha=self.alpha
     end
     'bc': begin
        self.bias=(0>x<1.)
        self.cont=(0>y<1.)*0.75

        xx=self.bias*255
        yy=self.cont*255

        slope=255./(2*yy)
        inter=-slope*(xx-yy)
        scl=0>(findgen(256)*slope+inter)<255
        if self.invert then scl=reverse(scl)
        
        self.oPalette->SetProperty,red=scl,green=scl,blue=scl

     end
     else:
  endcase
  self.oWindow->Draw,self.oView

end

pro segeditor::RightMode,event
  self.rightmode=event.select?'ag':'bc'
end

pro segeditor::SetMode,mode
  self.prevmode=self.mode
  self.mode=mode

 
  case self.mode of
     'track': widget_control,self.winst,set_value='move cursor to read '+$
                             'values and manipulate the images '+$
                             '(pan/zoom/stretch)'
     'delete': widget_control,self.winst,set_value='left click on regions '+$
                              'to delete regions'
     'merge': widget_control,self.winst,set_value='left click and drag to '+$
                             'merge two regions.'
     'erase': widget_control,self.winst,set_value='left click and drag to '+$
                             'erase pixels below the cursor'
     'draw': widget_control,self.winst,set_value='left click and drag to '+$
                            'draw a region, which will be automatically closed'
     'range': widget_control,self.winst,set_value='left click and drag, '+$
                             'and the pixels in the box will be '+$
                             'used to define the new range'
     'disassoc': widget_control,self.winst,set_value='left click to '+$
                                'disassociate an island region from its '+$
                                'parent region'
     'paint': widget_control,self.winst,set_value='left click and drag '+$
                             'to paint as a part of an existing region'
     else:
  endcase  
end


pro segeditor::ConvertXY,x0,y0,x,y,VIEW=view
  self.oView->GetProperty,view=view
  x=view[0]+x0*(view[2]/self.winsize)
  y=view[1]+y0*(view[3]/self.winsize)
end

pro segeditor::PositionText,RESIZE=resize
  if keyword_set(RESIZE) then begin
     self.oView->GetProperty,view=view
     char_dim=self.charsize*view[2:3]
  endif
  
 for i=0,self.oText->count()-1 do begin
     text=self.oText->Get(pos=i)
     text->GetProperty,uval=uval
     self.convertxy,uval[0,*],uval[1,*],x,y
     text->SetProperty,loc=[x,y],char_dim=char_dim
  endfor
end


pro segeditor::PanTo,x,y
  self.oView->GetProperty,VIEW=view
  x0=x-view[2]/2.-1 & x1=x+view[2]/2.+1
  y0=y-view[2]/2.-1 & y1=y+view[3]/2.+1


  x0=x-view[2]/2-1 & x1=x+view[2]/2+1
  y0=y-view[3]/2-1 & y1=y+view[3]/2+1

  view[0]=x0+1
  if x0 ge 0 then view[0]-=floor(x0)
  view[1]=y0+1
  if y0 ge 0 then view[1]-=floor(y0)
  self.oView->SetProperty,view=view

  ;update the fits tiling
  self->LoadTile,x0,x1,y0,y1

  self->PositionText
  
  self.oWindow->Draw,self.oView  
end

pro segeditor::Pan,event
  self->ConvertXY,event.x,event.y,x,y,view=view
  self.seg->GetProperty,tile=tile
  x+=tile[0] & y+=tile[1]

  self->PanTo,x,y
end


pro segeditor::Zoom,event
  self->ConvertXY,self.winsize/2,self.winsize/2,x,y,view=view
  zoom=event.clicks*self.zoomspeed
  zoomstate=self.zoomstate+zoom
  ;restrict the upper zoomstate to save the grphics rendering
  if zoomstate ge -2 && zoomstate lt 0.7 then begin 
     factor=10.^zoom

     self.seg->GetProperty,tile=tile
     x+=tile[0]
     y+=tile[1]
     dx=view[2]*factor
     dy=view[3]*factor
     x0=x-dx/2-1 & x1=x+dx/2+1
     y0=y-dy/2-1 & y1=y+dy/2+1
     
     view[0]=x0+1
     if x0 gt 0 then view[0]-=floor(x0)
     view[1]=y0+1
     if y0 gt 0 then view[1]-=floor(y0)
     
     view[2]=dx
     view[3]=dy
     self.oView->SetProperty,view=view
     
     ;first must put the old image
     self->LoadTile,x0,x1,y0,y1,OKAY=okay
     if okay then self.zoomstate=zoomstate

     self->PositionText,/resize
     self.oWindow->Draw,self.oView
  endif  
end

pro segeditor::xy2ad,x,y,a,d
  dx=x-(self.ast.crpix[0]-1)
  dy=y-(self.ast.crpix[1]-1)
  
  xsi=self.ast.cd[0,0]*dx+self.ast.cd[0,1]*dy
  eta=self.ast.cd[1,0]*dx+self.ast.cd[1,1]*dy
  wcsxy2sph,xsi,eta,a,d,ctype=self.ast.ctype,crval=self.ast.crval

end
  

pro segeditor::UpdateCoord,event
  self->ConvertXY,event.x,event.y,x,y,view=view
  self.xy=[event.x,event.y]

  ;(xx,yy) will be coord in the original frame
  ;(x,y) will be coord in the frame shown (according to self.ast)
  self.Seg->Value,x,y,xx,yy,s
  self.Img->Value,x,y,xx,yy,i

  if self.wcs then begin
     self->xy2ad,x,y,a,d
     case self.coord of
        'decimal': str=[string(a,f='(F0.8)'),string(d,f='(F0.7)')]
        'sexagesimal': begin
           a=sixty(a/15d)
           a=strjoin([' '+string(a[0],f='(I02)'),string(a[1],f='(I02)'),$
                      string(a[2],f='(F05.2)')],':')

           d=sixty(d)
           d=strjoin([string(d[0],f='(I+03)'),string(d[1],f='(I02)'),$
                      string(d[2],f='(F04.1)')],':')
           str=[a,d]
        end
        
        else: stop,'unknown RA/Dec coordinates'
     endcase
     self.oAD->SetProperty,string=['a: ','d: ']+str
  endif

  
;  self.oModel->SetProperty,hide=1
;  self.oWindow->Draw,self.oView,/create_instance
;  self.oImages->SetProperty,hide=1
;  self.oModel->SetProperty,hide=0
;  self.oView->SetProperty,/transparent
  self.oXY->SetProperty,string=['x: '+string(xx,f='(F0.1)'),$
                                'y: '+string(yy,f='(F0.1)')]

  
  ss=finite(s)?string(s,f='(I6)'):'NaN'
  ii=string(i,f='(F0.3)')
  self.oVal->SetProperty,string=[ss+": seg",ii+': img']
  


  self.oWindow->Draw,self.oView ;,/draw_instance
;  self.oImages->SetProperty,hide=0
;  self.oView->SetProperty,transparent=0

end

pro segeditor::Undo,event
  nundo=self.undoList->Get_count()
  if nundo eq 0 then return
  
  undo=self.undolist->Get_item()           ; get the data to undo
  self.redolist->Add,undo                  ; add to the redo list
  self.undolist->Delete                    ; remove last item 

  usens=(nundo-1 ne 0)
  rsens=(self.redoList->Get_Count() ne 0)
  widget_control,self.wundo,sens=usens
  widget_control,self.wclear,sens=usens || rsens
  widget_control,self.wredo,sens=rsens

  ; set the value to the image
  self->SetValues,undo.x,undo.y,undo.tile,undo.old 
end

pro segeditor::Redo,event
  nredo=self.redoList->Get_count()
  if nredo eq 0 then return

  redo=self.redolist->Get_item()           ; get the data to redo
  self.undolist->Add,redo                  ; add to the undo list  
  self.redolist->Delete                    ; remove the last item


  usens=(self.undoList->Get_Count() ne 0)
  rsens=(nredo-1 ne 0)
  widget_control,self.wundo,sens=usens 
  widget_control,self.wclear,sens=usens || rsens
  widget_control,self.wredo,sens=rsens
  self->SetValues,redo.x,redo.y,redo.tile,redo.new ; set the value to the image
end

pro segeditor::Clear,event
  widget_control,self.wundo,sens=0b
  widget_control,self.wclear,sens=0b
  widget_control,self.wredo,sens=0b
  self.redoList->Delete,/all
  self.undoList->Delete,/all
end
pro segeditor::SetUndo,undo
;  if self.undolist->Get_count() ge 50 then self.undolist->delete,0
  self.undolist->Add,undo
  widget_control,self.wundo,sens=1b
  widget_control,self.wclear,sens=1b  
end
pro segeditor::SetValues,x,y,tile,value
  self.saved=0b
  self.seg->Assign,x+tile[0],y+tile[1],value
  self->LoadTile,/last
  self.oWindow->Draw,self.oView
end
function segeditor::ValidXY,x,y
  return,x ge 0 && x lt self.ast.naxis[0] && y ge 0 && y lt self.ast.naxis[1]
end
pro segeditor::space,event
  n=n_elements(self.wleft)
  i=0
  repeat begin
     if widget_info(self.wleft[i],/button_set) then begin
        widget_control,self.wleft[i],set_button=0b
        ii=(i+1) mod n
        widget_control,self.wleft[ii],set_button=1b
        self->SetMode,widget_info(self.wleft[ii],/unam)
        quit=1b
     endif else quit=0b
  endrep until quit || (i++ eq n)
end


pro segeditor::DeleteRegion,event,ALL=all,SEGID=Segid
  
  npix=0
  if keyword_set(ALL) then begin
     gpix=where((*self.subseg) ne 0,npix)
  endif else begin
     if keyword_set(SEGID) then begin
        gpix=where((*self.subseg) eq segid,npix)
     endif else begin
        self.ConvertXY,event.x,event.y,x,y
        if self.ValidXY(x,y) then begin
           segid=(*self.subseg)[x,y]
           if segid ne 0 then gpix=where((*self.subseg) eq segid,npix)
        endif
     endelse
  endelse

  if npix ne 0 then begin
     self.seg->GetProperty,tile=tile
     xy=array_indices(self.ast.naxis,gpix,/dim)
     undo={x:reform(xy[0,*],npix),y:reform(xy[1,*],npix),$
           tile:tile,old:(*self.subseg)[gpix],new:replicate(0,npix)}
     self->SetUndo,undo                           ;set data to UNDO
     self->SetValues,undo.x,undo.y,tile,undo.new  ;set data to VIEW
  endif
end

pro segeditor::MergeRegions,event,START=start,DONE=done
  if keyword_set(START) then begin
     self->ConvertXY,event.x,event.y,x,y,VIEW=view
     if ~self.ValidXY(x,y) || ((*self.subseg)[x,y] eq 0) then return
     self.xy0=[x,y]
     self.oLine->SetProperty,hide=0b
     self.started=1b
     return
  endif
  if keyword_set(DONE) then begin
     self.oLine->GetProperty,data=data
     if ~array_equal(size(data,/dim),[2,2]) then goto,QUIT
     if ~self.ValidXY(data[0,1],data[1,1]) then goto,QUIT
     if data[0,0] lt 0 || data[1,0] gt self.ast.naxis[0]-1 && $
        data[0,1] lt 0 || data[1,1] gt self.ast.naxis[1]-1 then goto,QUIT
     
     new=(*self.subseg)[data[0,0],data[1,0]]
     old=(*self.subseg)[data[0,1],data[1,1]]
     if old eq 0 || new eq 0 || old eq new then goto,QUIT

     gpix=where(*self.subseg eq old,npix)
     if npix eq 0 then goto,QUIT
     xy=array_indices(self.ast.naxis,gpix,/dim)
     self.seg->GetProperty,tile=tile
     undo={x:reform(xy[0,*],npix),y:reform(xy[1,*],npix),$
           tile:tile,old:replicate(old,npix),$
           new:replicate(new,npix)}
     self->SetUndo,undo
     self->SetValues,undo.x,undo.y,undo.tile,undo.new

     QUIT:
     self.started=0b
     self.oLine->SetProperty,hide=1b
     self.oWindow->Draw,self.oView
     return
  endif

  if self.started then begin
     self->ConvertXY,event.x,event.y,x,y

;     self.oModel->SetProperty,hide=1
;     self.oWindow->Draw,self.oView,/create_instance
;     self.oImages->SetProperty,hide=1
;     self.oModel->SetProperty,hide=0
;     self.oView->SetProperty,/transparent

     self.oLine->SetProperty,data=[[self.xy0],[x,y]]
     self.oWindow->Draw,self.oView;,/draw_instance
;     self.oImages->SetProperty,hide=0
;     self.oView->SetProperty,transparent=0
  endif
end


pro segeditor::Erase,event
  self->ConvertXY,event.x,event.y,x,y,VIEW=view
  
  eraser=round(self.eraser*view[2:3]/100.)>1
  x0=floor(x-eraser[0])>0 & x1=ceil(x+eraser[0])<(self.ast.naxis[0]-1)
  y0=floor(y-eraser[1])>0 & y1=ceil(y+eraser[1])<(self.ast.naxis[1]-1)
  if x1 lt x0 || y1 lt y0 then return
  
  sub=(*self.subseg)[x0:x1,y0:y1]
  g=where(sub ne 0,npix)

  if npix ne 0 then begin
     xy=array_indices(sub,g)
     self.seg->GetProperty,tile=tile
     x=reform(xy[0,*],npix)+x0
     y=reform(xy[1,*],npix)+y0

     ;undo coordinates in the FITS format
     undo={x:x,y:y,tile:tile,old:sub[g],new:lonarr(npix)}
     self->SetUndo,undo                          ;set data to UNDO
     self->SetValues,undo.x,undo.y,tile,undo.new ;set data to VIEW
  endif
end

pro segeditor::DrawRegion,event,START=start,DONE=done
  if keyword_set(START) then begin
     self.ConvertXY,event.x,event.y,x,y
     self.oROI->SetProperty,data=[x,y,0],hide=0b
     self.started=1b
     return
  endif
  if keyword_set(DONE) then begin
     msk=self.oROI->ComputeMask(dim=self.ast.naxis)
     gpix=where(msk,npix)
     if npix ne 0 then begin
        self.seg->GetProperty,tile=tile
        new=self.seg->Max()+1
        xy=array_indices(self.ast.naxis,gpix,/dim)
        undo={x:reform(xy[0,*],npix),y:reform(xy[1,*],npix),$
              tile:tile,old:(*self.subseg)[gpix],$
              new:replicate(new,npix)}
        self->SetUndo,undo
        self->SetValues,undo.x,undo.y,undo.tile,undo.new
     endif

     self.started=0b
     self.oROI->SetProperty,hide=1b,data=0
     self.oWindow->Draw,self.oView
     return
  endif
  if self.started then begin
     self.oROI->GetProperty,data=data
     if n_elements(data) gt 1 then begin
        self.ConvertXY,event.x,event.y,x,y
        xy=[x,y,0]
        
        self.oROI->SetProperty,data=[[data],[xy]]        
        self.oWindow->Draw,self.oView;,/draw_instance
     endif
  endif
end

pro segeditor::DisassociateRegions,event
  self.ConvertXY,event.x,event.y,x,y
  if self.ValidXY(x,y) then begin
     old=(*self.subseg)[x,y]
     if old ne 0 then begin

        tmp=label_region((*self.subseg) ne 0)
        gpix=where(tmp eq tmp[x,y] and (*self.subseg) eq old,npix)        
        if npix ne 0 then begin
           new=self.seg->max()+1
           self.seg->GetProperty,tile=tile
           xy=array_indices(self.ast.naxis,gpix,/dim)          
           undo={x:reform(xy[0,*],npix),y:reform(xy[1,*],npix),$
                 tile:tile,old:replicate(old,npix),$
                 new:replicate(new,npix)}
           self->SetUndo,undo                          ;set data to UNDO
           self->SetValues,undo.x,undo.y,tile,undo.new ;set data to VIEW
        endif
     endif
  endif
end

pro segeditor::PaintRegion,event,START=start,DONE=done
  if keyword_set(START) then begin
     self->ConvertXY,event.x,event.y,x,y,VIEW=view

     if self->ValidXY(x,y) && (*self.subseg)[x,y] ne 0 then begin
        self.paintID=(*self.subseg)[x,y]
        self.started=1b
     endif
  endif
  if keyword_set(DONE) then begin

     self.started=0b

  endif
  if self.started then begin
     self->ConvertXY,event.x,event.y,x,y,VIEW=view
     paint=round(self.paint*view[2:3]/100.)>1
     x0=floor(x-paint[0])>0 & x1=ceil(x+paint[0])<(self.ast.naxis[0]-1)
     y0=floor(y-paint[1])>0 & y1=ceil(y+paint[1])<(self.ast.naxis[1]-1)
     if x1 lt x0 || y1 lt y0 then return
     
     sub=(*self.subseg)[x0:x1,y0:y1]
     gpix=where(sub ne self.paintid,npix)
     if npix ne 0 then begin
        xy=array_indices(sub,gpix)
        self.seg->GetProperty,tile=tile
        x=reform(xy[0,*],npix)+x0
        y=reform(xy[1,*],npix)+y0
         
        ;undo coordinates in the FITS format
        undo={x:x,y:y,tile:tile,old:sub[gpix],$
              new:replicate(self.paintid,npix)}

        self->SetUndo,undo                          ;set data to UNDO
        self->SetValues,undo.x,undo.y,tile,undo.new ;set data to VIEW
        self.oWindow->Draw,self.oView
     endif
  endif
end
     




pro segeditor::DrawRange,event,START=start,DONE=done
  if keyword_set(START) then begin
     self->ConvertXY,event.x,event.y,x,y,View=view
     self.xy0=[x,y]
     self.oBox->SetProperty,hide=0b
     self.started=1b

     ;instantiate
;     self.oModel->SetProperty,hide=1
;     self.oWindow->Draw,self.oView,/create_instance
;     self.oImages->SetProperty,hide=1
;     self.oModel->SetProperty,hide=0
;     self.oView->SetProperty,/transparent

     return
  endif
  if keyword_set(DONE) then begin
     self.oBox->GetProperty,data=data
     if ~array_equal(size(data,/dim),[3,5]) then goto,QUIT
     x0=floor(data[0,0]<data[0,1])>0
     x1=ceil(data[0,0]>data[0,1])<(self.ast.naxis[0]-1)
     y0=floor(data[1,1]<data[1,2])>0
     y1=ceil(data[1,1]>data[1,2])<(self.ast.naxis[1]-1)
     if x1 lt x0 || y1 lt y0 then goto,QUIT
     sub=(*self.subimg)[x0:x1,y0:y1]
     
     self->SetMinMax,sub
     self->SetImage
 
     
     QUIT:
     self.started=0b
     ;uninstantiate
;     self.oImages->SetProperty,hide=0
;     self.oView->SetProperty,transparent=0
     self.oBox->SetProperty,hide=1b
     self.oWindow->Draw,self.oView
     return
  endif

  if self.started then begin
     self->ConvertXY,event.x,event.y,x,y
    

     self.oBox->ReplaceData,[self.xy0[0],x,x,self.xy0[0]],$
                            [self.xy0[1],self.xy0[1],y,y]


     self.oWindow->Draw,self.oView;,/draw_instance


;     self.oWindow->Draw,self.oView
     return
  endif
end


pro segeditor::DoubleClick,event
  self.oImages->SetProperty,hide=1b
  oSel=(self.oWindow->Select(self.oView,[event.x,event.y],dim=[4,4]))[0]
  if obj_valid(oSel) then begin
     if obj_isa(oSel,'IDLgrTextEdit') then begin
        if obj_valid(self.oSel) then self.oSel->SetProperty,draw_cursor=0b
        self.oSel=oSel
        self.oSel->GetProperty,xrange=xr,yrange=yr,str=str
        x=(event.x-xr[0])/(xr[1]-xr[0])
        y=(event.y-yr[0])/(yr[1]-yr[0])
        start=self.oSel->GetIndexAtCoord(self.oWindow,x,y)
        self.old=str
        self.oSel->SetProperty,/draw_cursor,selection_start=start
     endif
  endif else begin
     if obj_isa(self.oSel,'IDLgrTextEdit') then begin
        self.oSel->SetProperty,draw_cursor=0b
        self.oSel=obj_new()
     endif
  endelse  
  self.oImages->SetProperty,hide=0b
end

pro segeditor::EditText,event
  self.oSel->GetProperty,name=name
  if name eq 'minimum' || name eq 'maximum' then begin
     ch=event.ch
     case ch of
        8: self.oSel->Delete
        127: self.oSel->Delete,/after
        13: begin
           self.oSel->GetProperty,string=str
           st='^[-+]?([0-9]+\.?[0-9]*|\.[0-9]+)([eEdD][-+]?[0-9]+)?$' ;F.P.
           q=stregex(str,st,/boolean)
           if q eq 0 then self.oSel->SetProperty,string=self.old

           val=float(str)
           if name eq 'minimum' then begin
              if val gt self.minmax[1] then begin
                 self.oSel->SetProperty,string=self.old
                 goto,QUIT
              endif
              self.minmax[0]=val
           endif else begin
              if val lt self.minmax[0] then begin
                 self.oSel->SetProperty,string=self.old
                 goto,QUIT
              endif
              self.minmax[1]=val
           endelse

           self->SetImage

           QUIT:
           self.oSel->SetProperty,draw_cursor=0b
           self.oSel=obj_new()
        end
        else: if (ch eq 43 || ch eq 45 || ch eq 46 || ch eq 101) || $
           (ch ge 48 && ch le 57) then self.oSel->Insert,string(ch)
     endcase
     self.oWindow->Draw,self.oView
  endif
end



pro segeditor::NonAscii,event  
  if obj_valid(self.oSel) then begin
     ;if here, then editing the text display
     case event.key of          
        5: begin
           self.oSel->GetProperty,selection_start=s
           self.oSel->SetProperty,selection_start=(s-1)>0
        end
        6: begin
           self.oSel->GetProperty,selection_s=s,str=str
           self.oSel->SetProperty,selection_s=(s+1)<strlen(str)
        end
        7: begin
           self.oSel->GetProperty,name=name,selection_start=s,string=str
           if name eq 'maximum' then begin
              self.oSel->SetProperty,draw_cursor=0b
              l0=strlen(str)
              self.oSel=self.oMin
              self.oSel->GetProperty,str=str
              l1=strlen(str)
              self.oSel->SetProperty,/draw_cursor,selection_st=0>(l1-l0+s)<l1
           endif
        end
        8: begin
           self.oSel->GetProperty,name=name,selection_start=s,string=str
           if name eq 'minimum' then begin
              self.oSel->SetProperty,draw_cursor=0b
              l0=strlen(str)
              self.oSel=self.oMax
              self.oSel->GetProperty,str=str
              l1=strlen(str)
              self.oSel->SetProperty,/draw_cursor,selection_st=0>(l1-l0+s)<l1
           endif                    
        end
        else: 
     endcase
     self.oWindow->Draw,self.oView
  endif else begin
     ;if here, then just moving the cursor around
     delta=1
;     case event.modifiers of
;        0: delta=1
;        1: delta=10
;        else: print,'fix modifier' ;this is tricky... you could pan
;        off the frame, so up must update hte view if you step too far
;     endcase
     case event.key of
        5: begin                ;left
           event.x=self.xy[0]-delta
           self->UpdateCoord,event
        end
        6: begin                ;right
           event.x=self.xy[0]+delta
           self->UpdateCoord,event
        end
        7: begin                ;up
           event.y=self.xy[1]+delta
           self->UpdateCoord,event
        end                     ;down
        8: begin
           event.y=self.xy[1]-delta
           self->UpdateCoord,event
        end
        else:
     endcase
  endelse
end



pro segeditor::DrawEvent,event
  case event.type of
     0: begin                   ;button press
        case event.press of
           1: begin             ;left
              case event.clicks of
                 1: begin
                    case self.mode of
                       'range': self->DrawRange,event,/start
                       'delete': self->DeleteRegion,event
                       'merge': self->MergeRegions,event,/start
                       'erase': self.started=1b
                       'draw': self->DrawRegion,event,/start
                       'disassoc': self->DisassociateRegions,event
                       'paint': self->PaintRegion,event,/start
                       else:
                    endcase
                 end
                 2: self->DoubleClick,event ;double click
                 else:
              endcase
           end                                
           2: self->Pan,event   ;middle
           4: begin
              ;instantiate
              ;self.oModel->SetProperty,hide=1
              ;self.oWindow->Draw,self.oView,/create_instance
              ;self.oImages->SetProperty,hide=1
              ;self.oModel->SetProperty,hide=0
              ;self.oView->SetProperty,/transparent              
              self->SetMode,'scale'
           end
           else: ;help,event,/str
        end
     end
     1: begin                   ;button release
        if event.release eq 2 then return ;ignore middle button release
        case self.mode of
           'scale': self->SetMode,self.prevmode
           'range': self->DrawRange,event,/done
           'merge': self->MergeRegions,event,/done
           'erase': self.started=0b
           'draw': self->DrawRegion,event,/done
           'paint': self->PaintRegion,event,/done
           else:
        endcase
     end
     2: begin                   ;mouse move
        case self.mode of
           'scale': self->ChangePalette,event
           'range': self->DrawRange,event
           'merge': self->MergeRegions,event
           'erase': if self.started then self->Erase,event
           'draw':  self->DrawRegion,event
           'paint': self->PaintRegion,event
           else:
        endcase
        self->UpdateCoord,event
     end
     5: begin
        if event.press then begin
           if ~obj_valid(self.oSel) then begin
              case event.ch of
                 32: self->Space,event              ;SPACE bar
                 81: self->Quit,event,/force        ;Q
                 100: self.oWindow->Draw,self.oView ;d
                 112: self->Print,event             ;p
                 113: self->Quit,event              ;q
                 114: self->Redo,event              ;r 
                 117: self->Undo,event              ;u
                 else: 
              endcase
           endif else self->EditText,event
        endif
     end
     6: begin                   ;non-ascii chars
        if event.press then self->NonAscii,event
     end
     7: self->zoom,event
     else:
  endcase
end




pro segeditor::LoadTile,x0,x1,y0,y1,OKAY=okay,LAST=last
  if ~keyword_set(LAST) then begin
     xx0=x0 & yy0=y0 & xx1=x1 & yy1=y1
  endif
  seg=self.seg->GetImage(xx0,xx1,yy0,yy1,header=header,dim=dim,LAST=last)


  
  okay=dim[0] ne 0
  if okay then begin
     *self.subimg=self.img->GetImage(x0,x1,y0,y1,LAST=last,header=header)
     *self.subseg=temporary(seg)

     self.oSeg->SetProperty,data=reform((*self.rgb)[*,*self.subseg],[3,dim])

     
     self.ast.naxis=size(*self.subimg,/dim)
     self.ast.crpix=sxpar(header,'CRPIX*')
     self.ast.ctype=sxpar(header,'CTYPE*')
     self.ast.crval=sxpar(header,'CRVAL*')
     
     self.ast.cd[0,0]=sxpar(header,'CD1_1',count=cd11)
     self.ast.cd[1,0]=sxpar(header,'CD2_1',count=cd21)
     self.ast.cd[0,1]=sxpar(header,'CD1_2',count=cd12)
     self.ast.cd[1,1]=sxpar(header,'CD2_2',count=cd22)

     if cd11 eq 0 then self.ast.cd[0,0]=sxpar(header,'PC1_1')
     if cd12 eq 0 then self.ast.cd[0,1]=sxpar(header,'PC1_2')
     if cd21 eq 0 then self.ast.cd[1,0]=sxpar(header,'PC2_1')
     if cd22 eq 0 then self.ast.cd[1,1]=sxpar(header,'PC2_2')
          
     self.wcs=array_equal(self.ast.ctype,['RA---TAN','DEC--TAN'])
     
     self->SetImage

  endif
end
pro segeditor::random,event,NOIMAGE=noimage
  *self.rgb=byte(randomu(seed,3,self.nrgb)*256)
  (*self.rgb)[*,0]=255b
  if ~keyword_set(NOIMAGE) then begin
     self.oSeg->SetProperty,$
        data=reform((*self.rgb)[*,*self.subseg],[3,self.ast.naxis])
     self.oWindow->Draw,self.oView
  endif
end


pro segeditor::SetMinMax,sub

  
  ;which pixels to analyze?
  if self.omit_zero then begin
     g=where(sub ne 0,n)
     if n eq 0 then return
     sub=sub[g]
  endif

  ;analyze the pixels
  case self.minmax_method of
     'minmax': mn=min(sub,max=mx,/nan)
     '[-3,+3] sigma': begin ;for images with lots of sources this is skewed
        ave=mean(sub,/nan)
        sig=stddev(sub,/nan)
        mn=ave-3*sig
        mx=ave+3*sig
     end
     '[-3,+10] sigma': begin
        ave=mean(sub,/nan)
        sig=stddev(sub,/nan)
        mn=ave-3*sig
        mx=ave+10*sig
     end
     else: begin
        print,'invalid minmax'
        return
     end
  endcase
  
  ;avoid zeros
  if mn eq mx then begin
     mn-=0.01
     mx+=0.01
  endif
  self.minmax=[mn,mx]
  self.oMin->SetProperty,string='min: '+string(self.minmax[0],f='(F0.3)')
  self.oMax->SetProperty,string='max: '+string(self.minmax[1],f='(F0.3)')
end

pro segeditor::Print,event
  psfile=dialog_pickfile(dialog_parent=self.base,file=self.psfile,$
                         filter='*.eps;*.ps',/fix_filter,/write)
  if psfile ne '' then begin  
     q=strsplit(psfile,'.',/ext,count=count)
     suffix=strlowcase(q[count-1])
     if suffix eq 'ps' || suffix eq 'eps' then begin
        self.psfile=psfile
        self.oWindow->GetProperty,image=image
        dim=size(image,/dim)
        xsize=dim[1]/100.
        ysize=dim[2]/100.
        
        set_plot,'ps'
        device,filename=self.psfile,/color,/cmyk,bits=8,$
               xsize=xsize,ysize=ysize,/inch
        tv,image,0,0,true=1
        device,/close
        set_plot,'x'
     endif
  endif  
end



pro segeditor::base,event,FORCE=force
  type=tag_names(event,/struc)
  case type of
     'WIDGET_KILL_REQUEST': segeditor_cleanup,event.top,FORCE=force
     'WIDGET_BASE': begin
        beep
        widget_control,self.base,xsize=self.dim[0],ysize=self.dim[1]
     end
     else: print,type
  endcase
end

pro segeditor::SetProperty,ZOOMSPEED=zoomspeed,$
                           ERASER=eraser,$
                           PAINT=paint,$
                           OMIT_ZERO=omit_zero,$
                           MINMAX_METHOD=minmax_method,$
                           MINMAX_COLOR=minmax_color,$
                           MINMAX_LINE=minmax_line,$
                           MINMAX_ALPHA=minmax_alpha,$
                           INVERT=invert,$
                           BACKGROUND=background,$    
                                ;LOGSCL=logscl,$
                           POWSCL=powscl,$
                           COORDS=coords,$
                           NRGB=nrgb,$
                           SCALE=scale,$
                           WINSIZE=winsize,$
                           SENSITIVITY=sensitivity,$
                           _REF_EXTRA=_re

  if n_elements(ZOOMSPEED) eq 1 then self.zoomspeed=zoomspeed
  if n_elements(ERASER) eq 1 then self.eraser=eraser
  if n_elements(PAINT) eq 1 then self.paint=paint
  if n_elements(OMIT_ZERO) eq 1 then self.omit_zero=keyword_set(omit_zero)
  if n_elements(MINMAX_METHOD) eq 1 then $
     self.minmax_method=self.minmaxs[minmax_method]
  if n_elements(MINMAX_COLOR) eq 3 then $
     self.oBox->SetProperty,color=minmax_color
  if n_elements(MINMAX_ALPHA) eq 1 then $
     self.oBox->SetProperty,alpha=minmax_alpha
  if n_elements(MINMAX_LINE) eq 1 then $
     self.oBox->SetProperty,linestyle=minmax_line
  if n_elements(BACKGROUND) eq 3 then self.oView->SetProperty,color=background
  if n_elements(POWSCL) eq 1 then self.powscl=powscl
  if n_elements(INVERT) eq 1 then self.invert=invert
  if n_elements(NRGB) eq 1 then self.nrgb=nrgb
  if n_elements(COORDS) eq 1 then self.coord=self.coords[coords]
  if n_elements(SCALE) eq 1 then self->SetImage,type=self.scales[scale]
  if n_elements(WINSIZE) eq 1 then self->Resize,winsize
  if n_elements(SENSITIVITY) eq 1 then widget_control,self.base,sens=sensitivity

  
  self->IDLitComponent::SetProperty,_EXTRA=_re

end
pro segeditor::GetProperty,SAVED=saved,$
                           ZOOMSPEED=zoomspeed,$
                           ERASER=eraser,$
                           PAINT=paint,$
                           OMIT_ZERO=omit_zero,$
                           MINMAX_METHOD=minmax_method,$
                           MINMAX_COLOR=minmax_color,$
                           MINMAX_ALPHA=minmax_ALPHA,$
                           MINMAX_LINE=minmax_line,$
                           INVERT=invert,$
                           BACKGROUND=background,$    
                                ;LOGSCL=logscl,$
                           POWSCL=powscl,$
                           COORDS=coords,$
                           SCALE=scale,$
                           NRGB=nrgb,$
                           WINSIZE=winsize,$
                           base=base,$
                           _REF_EXTRA=_re
  if arg_present(BASE) then base=self.base
  if arg_present(SAVED) then saved=self.saved
  if arg_present(ZOOMSPEED) then zoomspeed=self.zoomspeed
  if arg_present(ERASER) then eraser=self.eraser
  if arg_present(PAINT) then paint=self.paint
  if arg_present(OMIT_ZERO) then omit_zero=self.omit_zero
  if arg_present(INVERT) then invert=self.invert
  if arg_present(MINMAX_METHOD) then begin
     g=where(self.minmaxs eq self.minmax_method)
     minmax_method=g[0]
  endif
  if arg_present(MINMAX_COLOR) then self.oBox->GetProperty,color=minmax_color
  if arg_present(MINMAX_ALPHA) then self.oBox->GetProperty,alpha=minmax_alpha
  if arg_present(MINMAX_LINE) then self.oBox->GetProperty,line=minmax_line
  if arg_present(BACKGROUND) then self.oView->GetProperty,color=background
;  if arg_present(LOGSCL) then logscl=self.logscl
  if arg_present(POWSCL) then powscl=self.powscl
  if arg_present(NRGB) then nrgb=self.nrgb
  if arg_present(COORDS) then begin
     g=where(self.coords eq self.coord)
     coords=g[0]
  endif
  if arg_present(SCALE) then begin
     g=where(self.scales eq self.scale)
     scale=g[0]
  endif

  if arg_present(WINSIZE) then winsize=self.winsize
  
  if n_elements(_re) ne 0 then self->IDLitComponent::GetProperty,_EXTRA=_re
end


pro segeditor::cleanup
  obj_destroy,[self.oWindow,self.oView,self.oModel,self.oImg,self.oSeg,$
               self.oPalette,self.oBox,self.oLine,self.oROI,self.oSel,$
               self.seg,self.img,self.undolist,self.redolist,$
               self.region_stats,self.operations]
  ptr_free,self.subseg,self.subimg,self.rgb
end


function segeditor::init,segfile,imgfile,WINSIZE=winsize,$
                         GROUP_LEADER=group_leader
  if ~self->IDLitComponent::init(name='Seg Editor',descrip='Seg Editor') $
  then return,0b

  ;a basic check
  if segfile eq '' || ~file_test(segfile) then begin
     t=dialog_message(['Segmentation image is invalid:',segfile],/error,$
                      title='Invalid Segmentation',/center,dialog=group_leader)
     return,0b
  endif
  if imgfile eq '' || ~file_test(imgfile) then begin
     t=dialog_message(['Direct image is invalid:',imgfile],/error,$
                      title='Invalid Direct',/center,dialog=group_leader)
     return,0b
  endif
  
  minsize=540                   ;minimum size of the graphics window
  self.except=!except & !except=0
  self.quiet=!quiet & !quiet=1
  self.winsize=keyword_set(WINSIZE)?(winsize>minsize):minsize

;  self.winsize=540              ;size of the draw windows
;  self.winsize=787


  flat=1b                       ;make the buttons appear as flat?
  
  self.mode='track'             ;this is duplicated with self.wleft[0]
  self.bias=0.5
  self.cont=1.0
  self.invert=1b
  self.alpha=0.7
  self.top=255
  self.logscl=1000.
  self.powscl=2.
  self.rightmode='bc'
  self.zoomspeed=0.1
  self.omit_zero=1b
  self.minmax_method='minmax';'[-3,+3] sigma'
  self.eraser=0.05
  self.paint=0.05
  self.scale='log'
  self.saved=1b
  self.charsize=0.03
  self.margin=0.01
  self.nrgb=50000ul
  self.coord='sexagesimal'
  self.background=[205,255,255]
  
  ;basic options that will appear in the config windows
  self.scales=['log','linear','power law','sqrt','square','histo equal']
  self.minmaxs=['minmax','[-3,+3] sigma','[-3,+10] sigma']
  self.coords=['decimal','sexagesimal']
  
  self.base=widget_base(GROUP_LEADER=group_leader,title='SegEditor',$
                        /tlb_kill_request,/tlb_size_events,/tlb_resize_nodraw,$
                        uname='base',/column,uval=self,map=0b)
  

  kill=splash(["SegEditor",'v1.0','Russell Ryan','rryan@stsci.edu'],$
              title='About',group=self.base)

  ;load the bitmaps 
  psep=path_sep()
  path=expand_path(!path,/array)
  prog=file_search(path+psep+'segeditor__define.pro')
  fullpath=(prog[0] eq 'segeditor__define.pro')?'./':prog[0]
  self.install=file_dirname(fullpath)
  bmpdir=strjoin([self.install,'..','etc','bitmaps'],psep)+psep

  ;make the GUI
  tool=widget_base(self.base,/row,/tool,/align_center)
  files=widget_base(tool,/row,/toolbar)
  
  q=widget_button(files,value=bmpdir+'new.bmp',tooltip='New Segmentation Map',$
                  /bitmap,uname='new',flat=flat)
  q=widget_button(files,value=bmpdir+'save.bmp',$
                  tooltip='Save Segmentation Map',$
                  /bitmap,uname='saveas',flat=flat)

  guis=widget_base(tool,/row,/tool,space=0)
  q=widget_button(guis,value=bmpdir+'propsheet.bmp',$
                  tooltip='Edit Properties',$
                  /bitmap,uname='properties',uval=self,flat=flat)

  wtext=widget_button(guis,value=bmpdir+'text.bmp',$
                      tooltip='Edit Properties',$
                      /bitmap,uname='properties',flat=flat)
    
  
  control=widget_base(tool,/row,/toolbar,space=0)
  self.wundo=widget_button(control,value=bmpdir+'stepback.bmp',tooltip='Undo',$
                           /bitmap,uname='undo',sens=0b,flat=flat)
  self.undolist=obj_new('linkedlist')
  self.wclear=widget_button(control,value=bmpdir+'stop.bmp',sens=0b,$
                            tooltip='Clear History',/bitmap,$
                            uname='clear',flat=flat)
  self.wredo=widget_button(control,value=bmpdir+'step.bmp',tooltip='Redo',$
                           /bitmap,uname='redo',sens=0b,flat=flat)
  self.redolist=obj_new('linkedlist')

  ;track, delete, merge, erase, draw
  left=widget_base(tool,/row,/toolbar,/exclusive,space=0)
  self.wleft[0]=widget_button(left,value=bmpdir+'arrow.bmp',tooltip='Track',$
                              /bitmap,event_pro='segeditor_leftmode_event',$
                              uname='track',flat=flat)
  self.wleft[1]=widget_button(left,value=bmpdir+'select.bmp',tooltip='Delete',$
                              /bitmap,event_pro='segeditor_leftmode_event',$
                              uname='delete',flat=flat)
  self.wleft[2]=widget_button(left,value=bmpdir+'group.bmp',tooltip='Merge',$
                              /bitmap,event_pro='segeditor_leftmode_event',$
                              uname='merge',flat=flat)
  self.wleft[3]=widget_button(left,value=bmpdir+'button.bmp',tooltip='Erase',$
                              /bitmap,event_pro='segeditor_leftmode_event',$
                              uname='erase',flat=flat)
  self.wleft[4]=widget_button(left,value=bmpdir+'segpoly.bmp',tooltip='Draw',$
                              /bitmap,event_pro='segeditor_leftmode_event',$
                              uname='draw',flat=flat)
  self.wleft[5]=widget_button(left,value=bmpdir+'drawing.bmp',$
                              tooltip='Disassociate',/bitmap,$
                              event_pro='segeditor_leftmode_event',$
                              uname='disassoc',flat=flat)  
  self.wleft[6]=widget_button(left,value=bmpdir+'paint.bmp',$
                              tooltip='Paint Region',/bitmap,$
                              event_pro='segeditor_leftmode_event',$
                              uname='paint',flat=flat)
  self.wleft[7]=widget_button(left,value=bmpdir+'rectangl.bmp',$
                              tooltip='Set Range',/bitmap,$
                              event_pro='segeditor_leftmode_event',$
                              uname='range',flat=flat)
  for i=0,n_elements(self.wleft)-1 do begin
     uname=widget_info(self.wleft[i],/uname)
     widget_control,self.wleft[i],set_button=strcmp(uname,self.mode,/fold)
  endfor


  
  right=widget_base(tool,/row,/toolbar,/non,space=0)
  r=widget_button(right,value=bmpdir+'rgba.bmp',$
                  tooltip='Adjust the alpha channel',$
                  /bitmap,uname='rightmode',flat=flat)
  
  util=widget_base(tool,/row,/toolbar,space=0)
  q=widget_button(util,value=bmpdir+'dm.bmp',tooltip='Tabulate Regions',$
                  /bitmap,uname='tabulate',flat=flat)

  q=widget_button(util,value=bmpdir+'gears.bmp',/bitmap,$
                  tooltip='Other Image Operations',$
                  uname='operations',flat=flat)
  q=widget_button(util,value=bmpdir+'ran.bmp',/bitmap,$
                  tooltip='Set Random Coloring',uname='random',flat=flat)
  r=widget_button(util,value=bmpdir+'help.bmp',$
                  tooltip='Help Menu',/bitmap,uname='help',flat=flat)

  
  
  self.wdraw=widget_draw(self.base,xsize=self.winsize,ysize=self.winsize,$
                         /button,/motion,/view,/wheel,/expose,$
                         keyboard=2,graphics=2,retain=2,uname='draw',$
                         event_pro='segeditor_draw_event',uval=self)
  
  
  self.winst=widget_label(self.base,value=' ',/align_left,sunken=~flat,$
                          xsize=self.winsize-1.5)
  self->SetMode,self.mode


  ;to center the widget in the screen
  q=widget_info(self.base,/geom)
  device,get_screen_size=ss
  xoff=(ss[0]-q.xsize)/2
  yoff=(ss[1]-q.ysize)/2
  widget_control,self.base,xoff=xoff,yoff=yoff

  
  widget_control,self.base,/realize,map=1b
  widget_control,kill,show=1b
  
  ;https://metacpan.org/pod/X11::CursorFont 
  widget_control,self.wdraw,get_value=oWindow
  oWindow->SetCurrentCursor,standard=40 ;40=erase, 124=paint, 86=drawing
  self.oWindow=oWindow
  self.oView=obj_new('IDLgrView',view=[0,0,self.winsize,self.winsize],$
                    color=self.background)
  self.oImages=obj_new('IDLgrModel')
  self.oSeg=obj_new('IDLgrImage',blend=[4,3],alpha=self.alpha)
  self.oImg=obj_new('IDLgrImage')
  self.oImages->Add,self.oImg
  self.oImages->Add,self.oSeg
  self.oView->Add,self.oImages

  ;make the palettes
  p=bindgen(256)
  if self.invert then p=reverse(temporary(p))
  self.oPalette=obj_new('IDLgrPalette',red=p,green=p,blue=p)
  self.oImg->SetProperty,palette=self.oPalette

  self.rgb=ptr_new(/allocate)
  self->random,/noimage
    

  self.oModel=obj_new('IDLgrModel')
  self.oView->Add,self.oModel

  ;prepare for text fields
  self.oView->GetProperty,view=view
  textcolor=[255,128,0]
  char_dim=self.charsize*view[2:3]

  
  ;put text fields
  pos=fltarr(2,2)
  pos[0,*]=replicate(self.margin*self.winsize,2)
  pos[1,*]=self.winsize*(1-0.5*self.margin-[1,2]*self.charsize)
  self->ConvertXY,pos[0,*],pos[1,*],x,y

  self.oXY=obj_new('IDLgrText',strings=['x:','y:'],$
                   char_dim=char_dim,uval=pos,loc=[x,y],$
                   color=textcolor,align=0.,name='pixel coord',$
                   descrip='X/Y Coordinates',/register)

  
  pos=fltarr(2,2)
  pos[0,*]=replicate(self.margin*self.winsize,2)
  pos[1,*]=self.winsize*([2,1]*self.charsize-1.5*self.margin)
  self->ConvertXY,pos[0,*],pos[1,*],x,y

  self.oAD=obj_new('IDLgrText',strings=['',''],$
                   char_dim=char_dim,uval=pos,loc=[x,y],$
                   color=textcolor,align=0.,name='ra/dec',$
                   descrip='A/D Coordinates',/register)


  pos=fltarr(2,2)
  pos[0,*]=replicate(self.winsize*(1-self.margin),2)
  pos[1,*]=self.winsize*(1-0.5*self.margin-[1,2]*self.charsize)
  self->ConvertXY,pos[0,*],pos[1,*],x,y

  self.oVal=obj_new('IDLgrText',strings=[':seg',':img'],$
                    char_dim=char_dim,uval=pos,loc=[x,y],$
                    color=textcolor,align=1.,name='pix value',$
                    descrip='Image Values',/register)

  pos=fltarr(2,2)
  pos[0,*]=replicate(self.winsize*(1-self.margin),2)
  pos[1,*]=self.winsize*([2,1]*self.charsize-1.5*self.margin)
  self->ConvertXY,pos[0,*],pos[1,*],x,y

  self.oMin=obj_new('IDLgrTextEdit',strings=[''],$
                    char_dim=char_dim,uval=pos[*,0],loc=[x[0],y[0]],$
                    color=textcolor,align=1.,name='minimum',$
                    descrip='Min Value',/register)
  self.oMax=obj_new('IDLgrTextEdit',strings=[''],$
                    char_dim=char_dim,uval=pos[*,1],loc=[x[1],y[1]],$
                    color=textcolor,align=1.,name='maximum',$
                    descrip='Max Value',/register)

  
  self.oModel->Add,self.oXY
  self.oModel->Add,self.oAD
  self.oModel->Add,self.oVal
  self.oModel->Add,self.oMin
  self.oModel->Add,self.oMax

  self.oText=obj_new('IDL_Container')
  self.oText->Add,self.oXY
  self.oText->Add,self.oAD
  self.oText->Add,self.oVal
  self.oText->Add,self.oMin
  self.oText->Add,self.oMax


  texts=self.oText->Get(/all)
  widget_control,wtext,set_uval=texts

;  for i=0,n_elements(texts)-1 do begin
;     texts[i]->RegisterProperty,'hide',/boolean,name='Show'
;     texts[i]->RegisterProperty,'ALPHA_CHANNEL',/float,name='Opacity',$
;        valid=[0d,1d,0.01d]
;     texts[i]->RegisterProperty,'COLOR',/color,name='Color'
;     texts[i]->RegisterProperty,'fill_background',/boolean,$
;        name='Fill background'
;     texts[i]->RegisterProperty,'FILL_COLOR',/color,name='Color' 
;  endfor


  
  
  ;make hte ranger
  self.oBox=obj_new('range',hide=1b)
  self.oModel->Add,self.oBox

  ;make the Merger
  self.oLine=obj_new('IDLgrPolyline',hide=1b)
  self.oModel->Add,self.oLine

  ;make the drawer
  self.oROI=obj_new("IDLgrROI",hide=1b)
  self.oModel->Add,self.oROI

  widget_control,hourglass=1b
  
  ;initialize the images
  self.psfile='segeditor.ps'
  self.file=segfile
  self.seg=obj_new('fitsfile',segfile)
  self.img=obj_new('fitsfile',imgfile)

    
  
  self.subseg=ptr_new(/allocate)
  self.subimg=ptr_new(/allocate)
  
  ;load the images
  self.seg->GetProperty,dim=dim
  x0=(dim[0]-self.winsize)/2. & x1=x0+self.winsize
  y0=(dim[1]-self.winsize)/2. & y1=y0+self.winsize
  foo=self.img->AllImage()
  self.SetMinMax,temporary(foo)
  self->LoadTile,x0,x1,y0,y1


  self.oWindow->Draw,self.oView  
  
  widget_control,hourglass=0b


  ;register some data
  self->RegisterProperty,'eraser',/float,$
                         name='Eraser Size',valid=[0.1d,2d,0.01d],$
                         desc='size of eraser w.r.t. view'
  self->RegisterProperty,'paint',/float,$
                         name='Painter Size',valid=[0.1d,2d,0.01d],$
                         desc='size of papinter w.r.t. view'
  self->RegisterProperty,'ZOOMSPEED',/float,name='Zoom Speed',$
                         description='Speed of the zoom',$
                         valid_range=[0.005d0,0.5d,0.005d]

  self->RegisterProperty,'omit_zero',/boolean,name='Omit Zero',$
                         desc='omit zero in ranging'
  self->RegisterProperty,'minmax_method',name='Min/max Method',$
                         enum=self.minmaxs,$
                         desc='Statistics method for computing the range'
  self->RegisterProperty,'coords',name='RA/Dec Units',enum=self.coords,$
                         desc='Coordinates of the RA/Dec display'
  self->RegisterProperty,'scale',name='Scaling',enum=self.scales,$
                         desc='Scaling function for colors'
  self->RegisterProperty,'invert',name='Invert Colors',/boolean,$
                         desc='Invert the color scaling'
  self->RegisterProperty,'minmax_color',/color,name='Range Color',$
                         desc='color of the range drawing box'
  self->RegisterProperty,'minmax_alpha',/float,name='Range Opacity',$
                         desc='opacity of the range box',valid=[0d,1d,0.02d]
  self->RegisterProperty,'minmax_line',/linestyle,$
                         name='Range Linestyle',$
                         desc='Linestyle of the range drawing box' 
  self->RegisterProperty,'nrgb',/integer,name='RGB Number',$
                         desc='Number of RGB triplets in cache',$
                         valid=[10000L,1000000l,10000l]
  self->RegisterProperty,'powscl',/float,name='Powscl Base',$
                         desc='see the ds9 manual for the definition',$
                         valid=[0.1d,10d,0.05d]
  self->RegisterProperty,'background',/color,name='Background Color',$
                         desc='color of the background'

;  self->RegisterProperty,'winsize',/float,name='Window Size',$
;                         desc='size of the graphics window',$
;                         valid=[540d,maxsize,1d]


  if widget_info(kill,/valid) then widget_control,kill,/destroy


  ;get the size of the widget
  g=widget_info(self.base,/geom)
  self.dim=[g.xsize-2*g.xpad,g.ysize-2*g.ypad]
  xmanager,'segeditor',self.base,/no_block,cleanup='segeditor_cleanup'
  return,1b
end




pro segeditor__define
  _={RADEC,$
     naxis:lonarr(2),$
     crpix:dblarr(2),$
     ctype:strarr(2),$
     crval:dblarr(2),$
     cd:dblarr(2,2)}
  _={SEGEDITOR,$
     inherits IDLitComponent,$
     base:0l,$
     install:'',$
     dim:[0l,0l],$ 
     wop:0l,$
     winsize:0.,$
     wdraw:0l,$
     winst:0l,$
     wundo:0l,$
     wredo:0l,$
     wclear:0l,$
     wleft:lonarr(8),$
     oWindow:obj_new(),$
     oView:obj_new(),$
     oImages:obj_new(),$
     oModel:obj_new(),$
     oImg:obj_new(),$
     oSeg:obj_new(),$
     oPalette:obj_new(),$
     oBox:obj_new(),$
     oLine:obj_new(),$
     oROI:obj_new(),$
     oSel:obj_new(),$
     oXY:obj_new(),$
     oAD:obj_new(),$
     oVal:obj_new(),$
     oMin:obj_new(),$
     oMax:obj_new(),$
     oText:obj_new(),$
     seg:obj_new(),$
     img:obj_new(),$
     subseg:ptr_new(),$
     subimg:ptr_new(),$
     region_stats:obj_new(),$
     operations:obj_new(),$
     undolist:obj_new(),$
     redolist:obj_new(),$
     rgb:ptr_new(),$
     minmax:[0.,0.],$
     ast:{RADEC},$
     wcs:0b,$
     xy:[0,0],$
     xy0:[0.,0.],$
     time:0d,$
     number:0l,$

     minmaxs:strarr(3),$
     coords:strarr(2),$
     coord:'',$
     scales:strarr(6),$
     
     
     prevmode:'',$
     mode:'',$
     rightmode:'',$

     paintID:0l,$
     scale:'',$
     started:0b,$
     
     top:0b,$
     alpha:0.,$
     bias:0.,$
     cont:0.,$
     invert:0b,$
     logscl:0.,$
     powscl:0.,$
     eraser:0.,$
     paint:0.,$
     nrgb:0ul,$
     
     charsize:0.0,$
     margin:0.0,$
     old:'',$
     
     zoomstate:0d,$
     zoomspeed:0d,$
     background:[0b,0b,0b],$

     omit_zero:0b,$
     minmax_method:'',$

     psfile:'',$
     file:'',$
     saved:0b,$
     updateheader:0b,$
     except:0,$
     quiet:0l $
    }

end
     
  
