function fitsfile::MinMax
  mx=self->Max(min=mn)
  return,[mn,mx]
end
function fitsfile::Max,MIN=mn
  return,max(*self.data,MIN=mn,/nan)
end

pro fitsfile::Value,x,y,xx,yy,vv
  xx=x+self.tile[0]
  yy=y+self.tile[1]
  if xx gt 0 && xx lt self.dim[0] && yy gt 0 && yy lt self.dim[1] then $
     vv=(*self.data)[xx,yy] else $
        vv=!values.f_nan  
end
  
pro fitsfile::SXAddHist,hist,_EXTRA=_extra
  h=*self.header
  sxaddhist,h,hist,_extra=_extra
  *self.header=h
end
pro fitsfile::SXAddPar,name,value,comment,location,_EXTRA=_extra
  h=*self.header
  sxaddpar,h,name,value,comment,location,_EXTRA=_extra
  *self.header=h
end
function fitsfile::SXPar,name,abort,COUNT=count,COMMENT=comment,$
                         NOCONTINUE=nocontinue,SILENT=silent
  
  value=sxpar(*self.header,name,abort,COUNT=count,COMMENT=comment,$
              NOCONTINUE=nocontinue,SILENT=silent)
  return,value
end


pro fitsfile::Write,file
  writefits,file,*self.data,*self.header
end

function fitsfile::AllImage,HEADER=header
  if arg_present(HEADER) then header=*self.header
  return,*self.data
end

pro fitsfile::PutImage,img
  (*self.data)[self.tile[0]:self.tile[2],self.tile[1]:self.tile[3]]=img
end

function fitsfile::GetImage,x0,x1,y0,y1,ALL=all,HEADER=header,DIM=dim,LAST=last
  if ~keyword_set(LAST) then begin

     if keyword_set(ALL) then begin
        x0=0 & x1=self.dim[0]-1
        y0=0 & y1=self.dim[1]-1
     endif else begin        
        
        x0=floor(x0)>0
        x1=ceil(x1)<(self.dim[0]-1)
        y0=floor(y0)>0
        y1=ceil(y1)<(self.dim[1]-1)
        
        
        if x1 eq x0 then x1++
        if y1 eq y0 then y1++
     endelse
     
     self.tile=[x0,y0,x1,y1]

     
     if x0 gt x1 || y0 gt y1 then begin
        dim=[0,0]
        return,0
     endif
  endif

  if arg_present(DIM) then $
     dim=[self.tile[2]-self.tile[0]+1,self.tile[3]-self.tile[1]+1]

     
  if arg_present(header) then begin
     hextract,(*self.data),(*self.header),sub,header,$
              self.tile[0],self.tile[2],self.tile[1],self.tile[3],/sil
  endif else begin     
     sub=(*self.data)[self.tile[0]:self.tile[2],self.tile[1]:self.tile[3]]
  endelse

  return,sub
end

pro fitsfile::Assign,x,y,v
  (*self.data)[x,y]=v
end


;pro fitsfile::puttile,dat
;  (*self.data)[self.tile[0]:self.tile[2],self.tile[1]:self.tile[3]]=dat
;end
;
;function fitsfile::gettile,xc,yc,dx,dy,HEADER=header,LAST=last,TILEDIM=tiledim
;  if ~keyword_set(LAST) then begin
;     x0=(xc-dx/2)>0
;     x1=(x0+dx-1)<(self.dim[0]-1)
;     y0=(yc-dy/2)>0
;     y1=(y0+dy-1)<(self.dim[1]-1)
;
;     if x1 lt x0 || y1 lt y0 then stop,'tile invalid'
;     
;     self.tile=[x0,y0,x1,y1]
;
;  endif
;  if arg_present(TILEDIM) then tiledim=[x1-x0+1,y1-y0+1]
;  
;  if arg_present(header) then begin
;     hextract,(*self.data),(*self.header),sub,header,$
;              self.tile[0],self.tile[2],self.tile[1],self.tile[3],/sil
;  endif else begin
;
;     sub=(*self.data)[self.tile[0]:self.tile[2],self.tile[1]:self.tile[3]]
;  endelse
;  
;  return,sub
;end




pro fitsfile::GetProperty,file=file,data=data,header=header,dim=dim,TILE=tile
  if arg_present(file) then file=self.file
  if arg_present(data) then data=*self.data
  if arg_present(header) then header=*self.header
  if arg_present(dim) then dim=self.dim
  if arg_present(TILE) then tile=self.tile
end


pro fitsfile::cleanup
  ptr_free,self.data,self.header
end

function fitsfile::init,file,SILENT=silent;,NANVAL=nanval
  if ~file_test(file) then return,0b
  
  self.file=file
  data=readfits(self.file,header,SILENT=silent)
;  g=where(~finite(data),n)
;  if n ne 0 then begin
;     if ~keyword_set(NANVAL) then nanval=0
;     self.g=ptr_new(g,/no)
;     data[*self.g]=nanval
;  endif

  self.data=ptr_new(data,/no)
  self.header=ptr_new(header,/no)
  self.dim=size(*self.data,/dim)
  return,1b
end

pro fitsfile__define
  _={FITSFILE,$
     file:'',$
     data:ptr_new(),$
;     g:ptr_new(),$
     header:ptr_new(),$
     tile:[0l,0l,0l,0l],$       ;of the last tile
     dim:[0l,0l] $
    }
end
     
