


pro ps_event,event
  if ~strcmp(event.identifier,'',/fold) then begin
     value=widget_info(event.id,property_value=event.identifier,$
                       component=event.component)
     event.component->SetPropertyByIdentifier,event.identifier,value
  endif
end

pro ps_close,event
  widget_control,event.top,map=0b
  widget_control,event.top,/destroy
end

pro property_event,event
  ps_close,event
end


pro property,obj,GROUP_LEADER=group_leader,TITLE=title
  widget_control,hourglass=1b
  g=where(~obj_valid(obj),n,ncomplement=m)
  if n ne 0 then return
  if keyword_set(group_leader) && widget_info(group_leader,/valid) then begin
     g=widget_info(group_leader,/geom)
     xoff=g.xoffset+g.xsize
     yoff=g.yoffset
  endif else begin
     xoff=0
     yoff=0
  endelse

  
  
  if ~keyword_set(TITLE) then title='Property Editor'
  base=widget_base(title=title,/column,group_leader=group_leader,$
                   /tlb_kill_request,map=0b,xoffset=xoff,yoffset=yoff)

  props=obj[0]->QueryProperty()
  nprop=n_elements(props)

  
  
  prop=widget_propertysheet(base,value=obj,event_pro='ps_event',/multiple,$
                            ysize=nprop)
  if m ne 1 then begin
     g=widget_info(prop,/geom)
     widget_control,prop,xsize=g.xsize/2.*(m+1)
  endif
  close=widget_button(base,value='Close',event_pro='ps_close')


  widget_control,base,/realize,map=1b
  
  widget_control,hourglass=0b
 
  xmanager,'property',base,/no_block
end
