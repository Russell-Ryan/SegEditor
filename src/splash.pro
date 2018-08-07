pro splash_kill,id
  widget_control,id,get_uval=objects
  obj_destroy,objects
end
  

function splash,data,GROUP_LEADER=group_leader,title=title
  device,get_screen_size=ss

  dim=[500,344]
  dim=[300,150]
  
  off=(ss-dim)/2

;  img=read_bmp('../../experiment/t.bmp',r,g,b)
  
  base=widget_base(Title=title,GROUP_LEADER=group_leader,/column,$
                   kill_notify='splash_kill',tlb_frame_attr=13,$
                   xoff=off[0],yoff=off[1],xpad=0,ypad=0)
  draw=widget_draw(base,graphics=2,xsize=dim[0],ysize=dim[1])
  
  ;w=widget_label(base,value=strjoin(data,string(10b)),/sunk)
  widget_control,base,/realize

  widget_control,draw,get_value=oWindow
  oView=obj_new('IDLgrView',color=[192,192,192],view=[0,0,dim])
  oModel=obj_new('IDLgrModel')
;  oImage=obj_new('IDLgrImage',data=[[[r[img]]],[[g[img]]],[[b[img]]]],interleave=2)

  
  oView->Add,oModel
;  oModel->Add,oImage
  loc=fltarr(2,4)
  char_dim=[30,30]

  r=[0,255,255,127,255,127,255,127,128,255,  0,  0,255,255,255,255]
  g=[0,127,127,255,255,127,189,255,128,255,  0,255,  0,255,255,255]
  b=[0,127,255,255,127,255,127,127,128,255,255,233,  0,  0,  0,255]



  objects=obj_new("IDL_Container")
  objects->Add,oWindow
  objects->Add,oView
  objects->Add,oModel
;  objects->Add,oImage
  
;  loc[0,*]=204
;  loc[1,*]=129-findgen(4)*char_dim[1]
;  oText=obj_new('IDLgrText',string=data,loc=loc,char_dim=char_dim,$
;                color=[255,255,255],vertical_align=0.5,align=0.5)
;  oModel->Add,oText
;  objects->Add,oText
;
;  loc[0,*]=203
;  loc[1,*]=128-findgen(4)*char_dim[1]
;  oText=obj_new('IDLgrText',string=data,loc=loc,char_dim=char_dim,$
;                color=[255,255,0],vertical_align=0.5,align=0.5)
;  oModel->Add,oText
;  objects->Add,oText
;
;
;  loc[0,*]=202
;  loc[1,*]=127-findgen(4)*char_dim[1]
;  oText=obj_new('IDLgrText',string=data,loc=loc,char_dim=char_dim,$
;                color=[255,0,0],vertical_align=0.5,align=0.5)
;  oModel->Add,oText
;  objects->Add,oText
;
;  
;  loc[0,*]=201
;  loc[1,*]=126-findgen(4)*char_dim[1]
;  oText=obj_new('IDLgrText',string=data,loc=loc,char_dim=char_dim,$
;                color=[0,255,233],vertical_align=0.5,align=0.5)
;  oModel->Add,oText
;  objects->Add,oText
;



  loc[0,*]=dim[0]/2
  loc[1,*]=0.5*(dim[1]-4*char_dim[1])+findgen(4)*char_dim[1]


  oText=obj_new('IDLgrText',string=data,loc=loc,char_dim=char_dim,$
                color=[75,75,255],vertical_align=0.,align=0.5)
  oModel->Add,oText
  objects->Add,oText

  oWindow->Draw,oView

  widget_control,base,set_uval=objects

  
  return,base
end
