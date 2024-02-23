pro find_coefficients_3rd_poly_routine,result,azi,ele,x,y,poly_coe
  temp_fn=(poly_coe[0]+poly_coe[1]*ele+poly_coe[2]*ele^2+poly_coe[3]*ele^3)*sin(azi)
  temp_gn=(poly_coe[0]+poly_coe[1]*ele+poly_coe[2]*ele^2+poly_coe[3]*ele^3)*cos(azi)
  fn=transpose(temp_fn) & gn=transpose(temp_gn)
  E=intarr(1,n_elements(azi))+1
  Et=transpose(E) & xt=transpose(x) & yt=transpose(y)
  Tmatrix=invert([[Et##E,Et##x,Et##y],[Et##x,xt##x,xt##y],[Et##y,xt##y,yt##y]])
  coef_a=Tmatrix##([[Et],[xt],[yt]]##fn) & coef_b=Tmatrix##([[Et],[xt],[yt]]##gn)
  coe_matrix=[[transpose(coef_a)],[transpose(coef_b)]]
  matA=coe_matrix[1:2,*] & vecB=-1d*transpose(coe_matrix[0,*])
  ;;Solve (matA)vecX=vecB
  ludc,matA,tmp & vecX=lusol(matA,tmp,vecB)
  zenx=vecX[0] & zeny=vecX[1]
  result=create_struct('coef_a',coef_a,'coef_b',coef_b,'zenith_xy',[zenx,zeny],'fit_type','3rd_poly')
end