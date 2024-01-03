define i32 @fun(i32 %x, i32 %y) {
entry:
    br label %loop

loop:
    ; x1 = x or new_x
    %x1 = phi i32 [ %x, %entry ], [ %new_x, %if_x_greater ]
    ; y1 = y or new_y
    %y1 = phi i32 [ %y, %entry ], [ %new_y, %if_y_greater ]
    ; integer compare (icmp) not equal (ne)
    ; cmp = (x1 != y1)
    %cmp = icmp ne i32 %x1, %y1
    ; jump conditioal on %cmp to 'if_cond' or 'end' 
    br i1 %cmp, label %if_cond, label %end

if_cond:
    ; integer compare (icmp) signed greater than (sgt)
    ; x_gt_y = (x1 > y1)
    %x_gt_y = icmp sgt i32 %x1, %y1
    ; jump conditioal on %x_gt_y to 'if_x_greater' or 'if_y_greater' 
    br i1 %x_gt_y, label %if_x_greater, label %if_y_greater

if_x_greater:
    ; new_x = (x1 - y1)
    %new_x = sub i32 %x1, %y1
    ; unconditional jump to the top of loop
    br label %loop

if_y_greater:
    ; new_y = (y1 - x1)
    %new_y = sub i32 %y1, %x1
    ; unconditional jump to the top of loop
    br label %loop

end:
    ; return x1
    ret i32 %x1
}
