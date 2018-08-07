pro operations_event,event
  widget_control,event.handler,get_uval=self
  self->event,event
end

pro operations_kill,id
  widget_control,id,get_uval=self
  obj_destroy,self
end

pro operations::show
  widget_control,self.base,/show,map=1b
end

pro operations::MinNum,event
  self.main->GetSegMap,old
  h=histogram(old,reverse=ri)
  stop
end

pro operations::Compress,event
  widget_control,hourglass=1b
  widget_control,self.base,sens=0b
  self.main->SetProperty,sens=0b

  self.main->GetSegMap,old
  u=uniq(old,sort(old,/l64))
  new=value_locate(old[temporary(u)],old,/l64)

  gpix=where(old ne 0,npix)

  dim=size(old,/dim)


  xy=array_indices(dim,gpix,/dim)
  undo={x:reform(xy[0,*],npix),y:reform(xy[1,*],npix),$
        tile:[0L,0,dim[0]-1,dim[1]-1],old:old[gpix],new:new[gpix]}
  
  self.main->SetUndo,undo
  self.main->SetValues,undo.x,undo.y,undo.tile,undo.new ;set data to VIEW


  self.main->SetProperty,sens=1b
  widget_control,self.base,sens=1b
  widget_control,hourglass=0b
end


pro operations::dilate_erode,event,DILATE=dilate
  widget_control,hourglass=1b
  widget_control,self.base,sens=0b
  self.main->SetProperty,sens=0
  
  widget_control,event.id,get_uval=wslider
  widget_control,wslider,get_value=val
  kern=replicate(1,val,val)

  self.main->GetSegMap,old
  if keyword_set(DILATE) then begin
     new=dilate(old,kern,/gray,/ulong)
  endif else begin
     new=erode(old,kern,/gray,/ulong)
  endelse
  new=fix(new,type=size(old,/type))

  dim=size(old,/dim)
  gpix=where(new ne 0,npix)
  xy=array_indices(dim,gpix,/dim)
  undo={x:reform(xy[0,*],npix),y:reform(xy[1,*],npix),$
        tile:[0l,0l,dim[0]-1,dim[1]-1],old:old[gpix],new:new[gpix]}
  self.main->SetUndo,undo
  self.main->SetValues,undo.x,undo.y,undo.tile,undo.new

  self.main->SetProperty,sens=1b
  widget_control,self.base,sens=1b
  widget_control,hourglass=0b
end
pro operations::Close,event
  ;widget_control,event.top,/destroy
  widget_control,event.top,map=0b
end


pro operations::event,event
  type=tag_names(event,/str)
  case type of
     'WIDGET_BUTTON': begin
        uname=widget_info(event.id,/uname)
        case uname of 
           'compress': self->Compress,event
           'dilate': self->Dilate_Erode,event,/dilate
           'erode': self->Dilate_Erode,event
           'close': self->Close,event
           'minnum': self->MinNum,event
           else:
        endcase
     end
     else:
  endcase
end

function operations::init,main,DILATE=dilate,ERODE=erode,MINNUM=minnum
  main->GetProperty,base=group_leader
  if ~keyword_set(DILATE) then dilate=3
  if ~keyword_set(ERODE) then erode=3
  if ~keyword_set(MINNUM) then minnum=3
  
  

  if keyword_set(group_leader) && widget_info(group_leader,/valid) then begin
     g=widget_info(group_leader,/geom)
     xoff=g.xoffset+g.xsize
     yoff=g.yoffset
  endif else begin
     xoff=0
     yoff=0
  endelse

  
  self.base=widget_base(title='Operations',/column,group_leader=base,$
                        kill='operations_kill',xoff=xoff,yoff=yoff)
                        
  self.main=main
  
  r=widget_base(self.base,/row)
  b=widget_button(r,value='Compress',uname='compress',$
                  tooltip='Compress image to lowest positive integers')

  r=widget_base(self.base,/row)
  c=widget_base(r,/col,/align_bottom,xpad=0,xsize=50)
  b=widget_button(c,value='Dilate',uname='dilate',$
                  tooltip='Increase size of all regions.')
  c=widget_base(r,/col)
  w=widget_slider(c,min=1,max=100,value=dilate,xsize=120)
  widget_control,b,set_uval=w
  
  r=widget_base(self.base,/row)
  c=widget_base(r,/col,/align_bottom,xpad=0,xsize=50)
  b=widget_button(c,value='Erode',uname='erode',$
                  tooltip='Decrease size of all regions.')
  c=widget_base(r,/col)
  w=widget_slider(c,min=1,max=100,value=erode,xsize=120)
  widget_control,b,set_uval=w


  ;r=widget_base(self.base,/row)
  ;c=widget_base(r,/col,/align_bottom,xpad=0,xsize=50)
  ;b=widget_button(c,value='Remove',uname='minnum',$
  ;                tooltip='Remove sources smaller than this size.')
  ;c=widget_base(r,/col)
  ;w=widget_slider(c,min=1,max=100,value=minnum,xsize=120)
  ;widget_control,b,set_uval=w
                  

  
  
  r=widget_base(self.base,/row)
  b=widget_button(r,value='Close',uname='close',$
                  tooltip='Close Operations')

  widget_control,self.base,/realize,set_uval=self
  
;  widget_control,base,/realize
  xmanager,'operations',self.base,/no

  return,1b
end
  
pro operations__define
  _={OPERATIONS,$
     base:0l,$
     main:obj_new()}
end
