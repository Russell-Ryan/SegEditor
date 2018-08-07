pro range::GetProperty,data=data,LINESTYLE=linestyle,COLOR=color,ALPHA=alpha
  if arg_present(DATA) then self.oROI->GetProperty,data=data
  if arg_present(LINESTYLE) then self.oROI->GetProperty,linestyle=linestyle
  if arg_present(COLOR) then self.oPolygon->GetProperty,color=color
  if arg_present(ALPHA) then self.oPolygon->GetProperty,alpha=alpha
end
pro range::SetProperty,LINESTYLE=linestyle,COLOR=color,ALPHA=alpha,$
                       _REF_EXTRA=_re

  if n_elements(LINESTYLE) eq 1 then self.oROI->SetProperty,linestyle=linestyle
  if n_elements(COLOR) eq 3 then self.oPolygon->SetProperty,color=color
  if n_elements(ALPHA) eq 1 then self.oPolygon->SetProperty,alpha=alpha
  self->IDLgrModel::SetProperty,_EXTRA=_re
end


pro range::ReplaceData,x,y
  self.oROI->ReplaceData,[x,x[0]],[y,y[0]]
  self.oPolygon->SetProperty,data=transpose([[x],[y]])
end

pro range::cleanup
  obj_destroy,[self.oROI,self.oPolygon]
  self->IDLgrModel::cleanup
end

function range::init,LINESTYLE=linestyle,COLOR=color,ALPHA=alpha,_EXTRA=_e
  if ~self->IDLgrModel::init(_EXTRA=_e) then return,0b

  ;these are the 3 registered properties
  linestyle=(n_elements(LINESTYLE) eq 1)?linestyle:1
  color=(n_elements(COLOR) eq 3)?color:[100,100,255]
  alpha=(n_elements(ALPHA) eq 1)?alpha:0.3
 

  ;create the object  
  self.oROI=obj_new('IDLgrROI',linestyle=linestyle)
  self.oPolygon=obj_new('IDLgrPolygon',color=color,alpha=alpha)
  self->Add,self.oROI
  self->Add,self.oPolygon

  self->RegisterProperty,'linestyle',/linestyle,name='Line Style',$
                         desc='line style'

  self->RegisterProperty,'color',/color,name='Fill Color',desc='fill color'
  self->RegisterProperty,'alpha',/float,name='Opacity',desc='opacity',$
                         valid=[0d,1d,0.01d]
  
  return,1b
end


pro range__define
  _={RANGE,$
     inherits IDLgrModel,$
     oROI:obj_new(),$
     oPolygon:obj_new() $
    }
end
