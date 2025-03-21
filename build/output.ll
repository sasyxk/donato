; ModuleID = 'my_module'
source_filename = "my_module"

define double @eval() {
entry:
  %donato3 = alloca double, align 8
  %donato = alloca double, align 8
  %irene = alloca double, align 8
  store double 1.000000e+01, ptr %donato, align 8
  %donato_val = load double, ptr %donato, align 8
  br i1 false, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  %donato_val1 = load double, ptr %donato, align 8
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi double [ 3.000000e+00, %then ], [ %donato_val1, %else ]
  %addtmp = fadd double %donato_val, %iftmp
  %addtmp2 = fadd double 3.000000e+00, %addtmp
  store double %addtmp2, ptr %irene, align 8
  %irene_val = load double, ptr %irene, align 8
  %divtmp = fdiv double %irene_val, 2.000000e+00
  store double %divtmp, ptr %donato3, align 8
  %donato_val4 = load double, ptr %donato3, align 8
  %irene_val5 = load double, ptr %irene, align 8
  %gtmp = fcmp ogt double %donato_val4, %irene_val5
  br i1 %gtmp, label %then6, label %else7

then6:                                            ; preds = %ifcont
  ret double 1.000000e+02

else7:                                            ; preds = %ifcont
  %irene_val8 = load double, ptr %irene, align 8
  %negtmp = fneg double %irene_val8
  %donato_val9 = load double, ptr %donato3, align 8
  %subtmp = fsub double %negtmp, %donato_val9
  ret double %subtmp
}
