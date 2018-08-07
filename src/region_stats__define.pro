pro region_stats_event,event
  widget_control,event.handler,get_uval=self
  self->event,event
end

pro region_stats_kill,id
  widget_control,id,get_uval=self
  obj_destroy,self
end

pro region_stats::show
  widget_control,self.base,/show
end

pro region_stats::event,event
  type=tag_names(event,/str)
  case type of
     'WIDGET_TABLE_CELL_SEL': begin
        if event.sel_top eq 0 && event.sel_bottom eq self.nobj-1 && $
           event.sel_right eq event.sel_left && event.sel_left ne -1 then begin

           s=sort((*self.data).(event.sel_left))
           if event.sel_left eq self.last then begin
              self.increase=~self.increase
           endif else self.increase=0b
           self.last=event.sel_left
           if self.increase then s=reverse(s)
              
           (*self.data)=(*self.data)[s]
           widget_control,event.id,set_value=(*self.data),$
                          row_label=strtrim(s+1,2)
           return
        endif
        if event.sel_top eq event.sel_bottom && event.sel_left eq 0 && $
           event.sel_right ne 0 then begin
           obj=(*self.data)[event.sel_top]
           self.main->PanTo,obj.x,obj.y
        endif
        
     end
     'WIDGET_CONTEXT':
;        widget_displaycontextmenu,event.id,event.x,event.y,self.menu

     'WIDGET_BUTTON': begin
        case widget_info(event.id,/uname) of
           'delete': begin
              sel=widget_info(self.tab,/table_select)
              
              for i=sel[1],sel[3] do begin
                 self.main->DeleteRegion,SEGID=(*self.data)[i].segid
                 widget_control,self.tab,delete_rows=i
              endfor

           end
           else:
        endcase  
     end
     else: begin
        stop,type
     end
  endcase  
end




pro region_stats::SetImage,img
  widget_control,self.base,hourglass=1b
  npix=histogram(img,min=1,locations=segid,reverse=ri)
  nobj=n_elements(npix)
  data=replicate({REGION_STATS_DATA},nobj)
  data.segid=segid
  data.npix=npix

  for i=0,nobj-1 do begin
     case npix[i] of
        0:
        1: begin
           data[i].x=xy[0]
           data[i].y=xy[1]
        end          
        else: begin
           xy=array_indices(img,ri[ri[i]:ri[i+1]-1])
           xyc=total(xy,2)/npix[i]
           data[i].x=xyc[0]
           data[i].y=xyc[1]
        end
     endcase
  endfor

  g=where(npix ne 0,nobj)
  if nobj ne 0 then begin
     self.nobj=nobj
     data=data[g]
     
     format=strarr(4,self.nobj)
     format[0,*]='(I0)'
     format[1,*]='(I0)'
     format[2,*]='(F0.1)'
     format[3,*]='(F0.1)'


     widget_control,self.tab,set_value=data,format=format,$
                    ysize=self.nobj,row_labels=strtrim(lindgen(self.nobj)+1,2)
     *self.data=temporary(data)
  endif
  widget_control,self.base,hourglass=0b
end


pro region_stats::cleanup
  ptr_free,self.data
end

function region_stats::init,img,main,GROUP_LEADER=group_leader

  if keyword_set(group_leader) && widget_info(group_leader,/valid) then begin
     g=widget_info(group_leader,/geom)
     xoff=g.xoffset+g.xsize
     yoff=g.yoffset
  endif else begin
     xoff=0
     yoff=0
  endelse




  self.base=widget_base(title='Region Statistics',GROUP_LEADER=group_leader,$
                        uval=self,kill='region_stats_kill',xoff=xoff,yoff=yoff)
  self.main=main
  self.tab=widget_table(self.base,column_labels=['Seg ID','Npix','<x>','<y>'],$
                        /all_events,/row_major,xsize=4,/context)
  
  self.data=ptr_new(/allocate)

  widget_control,self.base,/realize
  
  if size(img,/n_dim) eq 2 then self->SetImage,img

  self.menu=widget_base(self.tab,/context_menu)
  w=widget_button(self.menu,value='Delete',uname='delete')


  
  self.last=-1
  xmanager,'region_stats',self.base,/no



  
  return,1b
end


pro region_stats__define
  _={REGION_STATS_DATA,$
     segid:0l,$
     npix:0l,$
     x:0.,$
     y:0.}
  
  _={REGION_STATS,$
     base:0l,$
     menu:0l,$
     tab:0l,$
     nobj:0l,$
     increase:0b,$
     last:0,$
     main:obj_new(),$
     data:ptr_new()}

end
