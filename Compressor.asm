;You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt

org 100h
jmp start        

    create       db 'testout.txt'     
     
    file_to_encode db 256 dup(?)
    file_to_decode db 256 dup(?)
     
    ;File Handles
    out_h        dw ?    ;Create File Handle
    open_h        dw ?    ;Open File Handle
    close_h       dw ?    ;Close File Handle

    frequency    dw 1024   dup(0)
    upkid        dw 1024   dup(0)
    downkid      dw 1024   dup(0)
    parent       dw 1024   dup(0)
    code         dw 1024   dup(?)
	
    root         dw 400h                                                              
    last_point   dw 0  
    min_pos1     dw 0            ;Position of first minimum to be initialized
    min_pos2     dw 0            ;Position of second minimum to be initialized
                         
    ;Declare message 1024 -- should we set this higher later?
    message      dw 'This is a test',0  
    message_r     dw ?  
    
    ;Encoded message -- message to be encoded
    encode_message    dw 1024 dup(?) 
    encode_message_ptr dw 0  
    encode_message_lnt dw 0               
    
    ;Encode the input
    bits         db ?
    bitcount     dw ?  
    endbitcount  dw ?    
    
    huffman_msg  dw 'HUFFMAN $'              
    parse_message_error dw "Error with command. Unrecognized instructions. $"
    create_file_msg_error dw "Error with creating file. Must be under 8 chars!. $"
    
    min_count    dd 0   
    total        dd 0            ;BYTE LIMITATION IS 65536. TOTAL CANNOT BE MORE!
    value_min_p1 dd 0            ;Value of what's in position 1
    value_min_p2 dd 0            ;Value of what's in position 2    
    min_count_t  dd 0       
    
    word_segment dw ?            ;These indicate the offset and the segments
    word_offset  dw ?       
                          
    error_message dw 'Error at: ',0
    
    ;This is for encode
    i            dw 0   
    old_i        dw 0
    
    
_start:   
    call _parse_command_file    
    call _open_file                                 
    call _create_file     
    call _initialize   
    call _write_table     
    call _total_freq 
    call _huffman_body                                             
    jmp exit  
    
;----------------PARSING COMMAND LINE------------------------
;
;------------------------------------------------------------
_parse_command_file:
    push ax
    push bx
    push cx
    push dx 
    push si 
    push di
    mov  si, 81h
    mov  di, offset _file_to_encode 
	
	
_l1: 
    xor ax, ax
    mov al, byte ptr [si] 
    inc si
    cmp al, 0Dh
    je  _error_parse_file
    cmp al, 20h ;Space 
    je  _l1  
	

_l2:                                         ;Input file                
    mov  byte ptr [di], al
    inc  di
    mov  al, byte ptr [si]  
    inc  si
    cmp  al, 0dh  
    je   _error_parse_file
    cmp  al, 20h 
    jne  _l2
    inc  di
    mov  byte ptr [di], 0                     ;Null terminated string
    mov  di, offset _file_to_decode   
     
    
_l3:
    mov  al, byte ptr [si]   
    inc  si 
    cmp  al, 0dh
    je   _end_parse
    mov  byte ptr [di], al
    inc  di      
    jmp  _l3
    
    
_end_parse:  
    inc  di
    mov  byte ptr [di], 0 
    pop  di   
    pop  si
    pop  dx
    pop  cx
    pop  bx
    pop  ax
    ret   
	
 
_error_parse_file:  
    xor  ax, ax
    mov  ah, 9h
    mov  dx, offset _parse_message_error
    int  21h
    jmp  exit
;----------------PARSING COMMAND LINE------------------------
;
;------------------------------------------------------------
                                       

;----------------OPEN FILE-----------------------------------
;
;------------------------------------------------------------
_open_file:
    push ax
    push cx
    push dx
    mov  ah, 3Dh
    mov  al, 0h
    mov  dx, offset _file_to_encode
    sub  cx, cx
    int  21h
    jc   _open_file_error
    mov  _open_h, ax 
    pop  ax
    pop  cx
    pop  dx
    ret

	
_open_file_error:
    pop  dx
    pop  cx
    pop  ax 
    ret                        
;----------------OPEN FILE-----------------------------------
;
;------------------------------------------------------------
       
     
;------------------CREATE FILE---------------------------------
;
;--------------------------------------------------------------    
_create_file:     
    push ax
    push cx
    push dx
    mov  ah, 3ch
    sub  cx, cx
    mov  dx, offset file_to_decode
    int  21h
    jc   _create_file_error
    mov  out_h, ax
    pop  ax
    pop  cx
    pop  dx
    ret
	

_create_file_error:  
    xor  ax, ax   
    xor  dx, dx
    mov  ah, 9h
    mov  dx, offset create_file_msg_error
    int  21h
    jmp  exit
;------------------CREATE FILE---------------------------------
;
;--------------------------------------------------------------


;------------------WRITE TABLE----------------------------------
;
;--------------------------------------------------------------  
_write_table:             
    push ax
    push cx
    push dx
    push bx
    mov  ah, 40h
    mov  bx, out_h  
    mov  cx, 2048
    mov  dx, offset frequency
    int  21h
    pop  bx
    pop  dx
    pop  cx
    pop  ax  
    ret
;------------------WRITE TABLE----------------------------------
;
;--------------------------------------------------------------  
       

;------------------initialize FREQUENCY TABLE------------------    
;This method initializes the frequency table. Counts the 
;presence of each symbol encountered.
;--------------------------------------------------------------
_initialize:        
    push bx
    push di
    push bp
    push ax
    push dx 
    push si
    mov  di, offset frequency	;Set di to the position of frequency 
    mov  si, offset min_count	;Set si to the position of min_count
    
    
_inl:             
    xor  ax, ax					;0 out ax register
    mov  ah, 3Fh				;Instruction to read file
    mov  bx, open_h				;Give it the file to be read file handle
    mov  cx, 1					;cx = 1 number of bytes to be read
    mov  dx, offset message_r	;Store result in message_r
    int  21h          
    cmp  ax, cx					;EOF
    jne  _done_init				;Jump to done_init if finished     
    xor  dx, dx              
    mov  dx, message_r
    mov  ax, dx					;Move the byte in di to al 
    xor  ah, ah					;0 out ah part       
    mov  bp, 4					;Multiply by 4 since frequency is a double word
    mul  bp                                     
    mov  bp, ax					;put the result in bp        
    ;inc word ptr [di+bp]		;Increment the frequency[pos] by 1
    mov  ax, word ptr [di+bp]    
    add  ax, 1					;This checks to see if there is overflow and carry bit set 
    ;Account for overflow. If carry bit is set then we add one to the next word over
    jnc  _l_than_65535
    mov  bx, word ptr [di+bp+2]
    add  bx, 1
    mov  word ptr [di+bp+2], bx	;Little endian, add 1 to the next word..part of it.    
  

_l_than_65535:                                                                                     
    mov  word ptr [di+bp], ax
    mov  ax, word ptr [si+2]	;word_segment is set to first 2 bytes of min_count
    mov  word_segment, ax
    mov  ax, word ptr [si]		;word_offset is set to second 2 bytes of min_count
    mov  word_offset, ax
    mov  dx, word ptr [di+bp+2]	;If first two bytes of frequency[pos]>min_count then set values
    cmp  dx, word_segment
    ja   _inle  
    cmp  dx, word_segment		;If first two bytes of frequency[pos]<min_count then skip to start
    jb   _inl				                  	
    mov  dx, word ptr [di+bp]
    cmp  dx, word_offset
    ja   _inle
    jmp  _inl
    

_inle:     
    mov  ax, word ptr [di+bp]	;Set the second two bytes of frequency[pos]=min_count
    mov  word ptr [si], ax     
    mov  ax, word ptr [di+bp+2]	;Set the first two bytes of frequency[pos]=min_count
    mov  word ptr [si+2], ax   
    inc  bx
    jmp  _inl    

	
_done_init:   
    pop  si
    pop  dx
    pop  ax
    pop  bp
    pop  di
    pop  bx  
    ret
;------------------initialize FREQUENCY TABLE------------------    
;This method initializes the frequency table. Counts the 
;presence of each symbol encountered.
;--------------------------------------------------------------
 

;------------------COUNT TOTAL FREQUENCY-----------------------
;Goes through the list and finds the TOTAL FREQUENCY
; MAXES OUT AT AROUND 4294967296 BYTES OR 4 MB
;-------------------------------------------------------------- 
_total_freq:    
    push ax
    push bx
    push si
    push di
    push cx           
    mov  si, offset frequency	;Set si to the position of frequency
    mov  di, offset total		;Set di to the position of total 
    mov  cx, 256				;Size of the frequency table
	
 
_tfl: 
    ;This sets the first Word of the 4 byte word
    mov  ax, word ptr total		;Set ax to value of total
    mov  bx, word ptr [si]		;Set bx to value of SI
    add  ax, bx  	
    mov  total, ax				;Add them together    
    jnc  _l_than_65535_c		;Carry has been detected 
    inc  word ptr [di+2]		;increment whatever is in total by 1

	
_l_than_65535_c:                                        					
    mov  ax, word ptr [di+2]	;Set ax to the value of whatever is in total
    mov  bx, word ptr [si+2]   	;Set bx to the value of whatever is in second word in frequency table
    add  ax, bx
    mov  [di+2], ax     
    add  si, 4					;Increment si by 4...frequency table is spaced by double words
    loop _tfl       
    pop  cx 
    pop  di
    pop  si
    pop  bx
    pop  ax  
    ret                                                                                    
;------------------END COUNT TOTAL FREQUENCY--------------------
;Goes through the list and finds the TOTAL FREQUENCY
; MAXES OUT AT AROUND 4294967296 BYTES OR 4 MB
;---------------------------------------------------------------    
                                                                                                                                     

;------------------HUFFMAN BODY--------------------------------
;This is the main body for the Huffman Function
;
;-------------------------------------------------------------- 
_huffman_body:     
    push ax
    push bx
    push cx
    push dx
    push si
    push di    
    sub  bx, bx       
    mov  si, offset total                                                            
    mov  di, offset min_count
    mov  bx, word ptr [si]		;Set min_count equal to total
    mov  word ptr [di],   bx                   
    mov  bx, word ptr [si+2]
    mov  word ptr [di+2], bx  
    
                
_huff_out_loop:       
    sub  bx, bx
    mov  si, offset min_count
    mov  di, offset min_count_t
    mov  bx, word ptr [si]		;Set value of min_count_temp = min_count
    mov  word ptr [di],bx  
    mov  bx, word ptr [si+2]
    mov  word ptr [di+2], bx
    mov  si, offset value_min_p1 ;Set value of value_min_p1 = 0
    mov  word ptr [si], 0
    mov  word ptr [si+2], 0
    mov  si, offset value_min_p2 ;Set value of value_min_p2 = 0
    mov  word ptr [si], 0       
    mov  word ptr [si+2], 0 
      
;----------FIND MIN POS 1 ROUTINE------------------------
;Find the position of the first minimum and it's value
;-------------------------------------------------------- 
    xor  dx, dx
    xor  cx, cx
    mov  ax, root
    mov  bx, 4
    div  bx
    mov  cx, ax					;Loop condition. Root/4. Starts at 256 
    xor  si, si
    mov  si, offset frequency	;si = frequency[pos]  
    mov  di, offset min_count_t	;di = min_count_t pos 
    xor  ax, ax
	

_min_pos_1:     
    sub  bx, bx					;word_segment = min_count_t (1st 2 bytes)
    mov  bp, ax       
    mov  bx, word ptr [di+2]
    mov  word_segment, bx			 
    mov  bx, word ptr [di]		;word_offset  = min_count_t (2nd 2 bytes)
    mov  word_offset,  bx 
    mov  dx, word ptr [si+bp+2]		                  
    cmp  dx, 0
    jz   _c1					;frequency[pos+2] == 0 check frequency[pos] (1st 2 bytes)
    cmp  dx, word_segment
    jb 	 _huff_c1				;frequency[pos+2] <  min_count_t (1st 2 bytes) Change min_count values
    cmp  dx, word_segment
    ja   _c2					;frequency[pos+2] >  min_count_t (1st 2 bytes) Loop again       
    
	
_c1:
    mov  dx, word ptr [si+bp]		                 
    cmp  dx, 0				 
    jz   _c2					;frequency[pos] == 0 check frequency[pos] (2nd 2 bytes) Loop again  
    cmp  dx, word_offset
    jb   _huff_c1				;frequency[pos] <  min_count_t (2nd 2 bytes) Change min_count values

	
_c2:
    add  ax, 4
    loop _min_pos_1 
    jmp  _d_min_1    
      
	  
_huff_c1:       
    mov  dx, word ptr [si+bp]	;min_count_t 2nd bytes = frequency[pos]
    mov  word ptr [di], dx   
    mov  dx, word ptr [si+bp+2] ;min_count_t 1st bytes = frequency[pos+2]		
    mov  word ptr [di+2], dx
    mov  min_pos1, ax     
    add  ax, 4
    loop _min_pos_1
    ;----------FIND MIN POS 1 ROUTINE------------------------
    ;Loop Back
    ;--------------------------------------------------------


_d_min_1:    
    mov  di, offset value_min_p1    					
    sub  bp, bp					;Position of the first minimal                
    mov  bp, min_pos1    
    sub  ax, ax                     
    mov  ax, word ptr [si+bp]	;Value_min_p1 = frequency[pos]         
    mov  word ptr [di], ax
    mov  ax, word ptr [si+bp+2]          
    mov  word ptr [di+2], ax    
    mov  word ptr [si+bp], 0	;Clear the Values at frequency[min_pos1]
    mov  word ptr [si+bp+2], 0
    mov  di, offset min_count                     
    mov  bx, word ptr [di]		;Set value of min_count_temp = min_count
    mov  di, offset min_count_t
    mov  word ptr [di],bx 
    mov  di, offset min_count
    mov  bx, word ptr [di + 2]
    mov  di, offset min_count_t
    mov  word ptr [di+2], bx               
    ;----------FIND MIN POS 2 ROUTINE------------------------
    ;Find the position of the second minimum and it's value
    ;--------------------------------------------------------   
    xor  dx, dx
    xor  cx, cx
    mov  ax, root
    mov  bx, 4
    div  bx
    mov  cx, ax
    xor  si, si
    mov  si, offset frequency	;si = frequency[pos]  
    mov  di, offset min_count_t	;di = min_count_t pos         
    xor  ax, ax

	
_min_pos_2:    
    sub  bx, bx					;word_segment = min_count_t (1st 2 bytes)
    mov  bp, ax       
    mov  bx, word ptr [di+2]
    mov  word_segment, bx		
    mov  bx, word ptr [di]		;word_offset  = min_count_t (2nd 2 bytes)
    mov  word_offset,  bx 
    mov  dx, word ptr [si+bp+2]		                  
    cmp  dx, 0
    jz   _c_12					;frequency[pos+2] == 0 check frequency[pos] (1st 2 bytes)
    cmp  dx, word_segment
    jb 	 _huff_c2				;frequency[pos+2] <  min_count_t (1st 2 bytes) Change min_count values
    cmp  dx, word_segment
    ja   _c_22					;frequency[pos+2] >  min_count_t (1st 2 bytes) Loop again

	
_c_12:
    mov  dx, word ptr [si+bp]		                 
    cmp  dx, 				 
    jz   _c_22					;frequency[pos] == 0 check frequency[pos] (2nd 2 bytes) Loop again  
    cmp  dx,	 word_offset
    jb   _huff_c2				;frequency[pos] <  min_count_t (2nd 2 bytes) Change min_count values

	
_c_22:
    add  ax, 	 4
    loop _min_pos_2 
    jmp  _d_min_2     
    
	
_huff_c2: 
    mov  dx, word ptr [si+bp]	;min_count_t 2nd bytes = frequency[pos]
    mov  word ptr [di], 	 dx   
    mov  dx, word ptr [si+bp+2] ;min_count_t 1st bytes = frequency[pos+2]		
    mov  word ptr [di+2], dx
    mov  min_pos2, ax     
    add  ax, 	 4
    loop _min_pos_2 
    ;----------FIND MIN POS 2 ROUTINE------------------------
    ;Loop Back
    ;--------------------------------------------------------  

_d_min_2:   
    mov  di, offset value_min_p2    					
    sub  bp, bp					;Position of the first minimal                
    mov  bp, min_pos2    
    sub  ax, ax            
    mov  ax, word ptr [si+bp]	;Value_min_p2 = frequency[pos]         
    mov  word ptr [di], ax
    mov  ax, word ptr [si+bp+2]          
    mov  word ptr [di+2], ax    
    mov  word ptr [si+bp], 0	;Clear the Values at frequency[min_pos2]
    mov  word ptr [si+bp+2], 0
    sub  bp, bp                                                                    
    mov  bp, root                                     
    
   ;---------upkid-------------------
    mov  si, offset upkid		;Set upkid[pos] = min_pos_1 (position of first minimal)
    sub  bx, bx
    mov  bx, min_pos1            
    mov  word ptr [si+bp], bx                   
    ;---------upkid-------------------
    
    ;---------downkid-----------------
    mov  si, offset downkid		;Set downkid[pos] = min_pos_2 (position of second minimal) 
    sub  bx, bx
    mov  bx min_pos2
    mov  word ptr [si+bp], bx                   
    ;---------downkid-----------------   
    
    ;---------frequency root----------
    mov  si, offset frequency 
    mov  di, offset value_min_p1 ;Add value_min_p1 and value_min_p2 and get the total (1st bytes)
    sub  bx, bx
    mov  bx, word ptr [di]   
    mov  di, offset value_min_p2
    sub  ax, ax
    mov  ax, word ptr [di]
    add  ax, bx
    jnc  _l_than_65535_cf		;If carry exists, increment 1st set of bytes
    mov  bx, word ptr [si+bp+2]
    add  bx, 1
    mov  word ptr [si+bp+2], bx
	
      
_l_than_65535_cf:
    mov  word ptr[si+bp], ax
    mov  di, offset value_min_p1 ;bx = value_min_p1 high order
    sub  bx, bx
    mov  bx, word ptr [di+2]   
    mov  di, offset value_min_p2 ;ax = value_min_p2 high order
    sub  ax, ax
    mov  ax, word ptr [di+2]
    add  ax, bx					 ;ax = ax + bx             
    mov  bx, word ptr [si+bp+2]  ;bx = frequency[pos+2]            
    add  ax, bx                  ;ax = ax + bx
    mov  word ptr [si+bp+2], ax  ;frequency[pos+2] = ax               
    ;---------frequency root----------    
    sub  bx, bx
    mov  bx, root
    ;---------change parent 1/2---------                                  
    mov  si,	offset parent 
    sub  bp, bp				 	;Parent of min_pos1
    mov  bp, min_pos1
    mov  word ptr [si+bp], bx
    sub  bp, bp				 	;Parent of min_pos2
    mov  bp, min_pos2
    mov  word ptr [si+bp], bx
    ;---------change parent 1/2---------                                  
    mov  last_point, bx
    add  bx, 4
    mov  root, bx    
    mov  si, offset frequency
    mov  di, offset total
    mov  bp, last_point
    mov  bx, word ptr [si+bp+2]	;bx = frequency[current_pos] 1st set of bytes
    mov  ax, word ptr [di+2]	;ax = total 1st set of bytes
    cmp  bx, ax					 
    jb  _huff_out_loop			;frequency[current_pos]<total 1st set of bytes Re-Loop 
    cmp  bx, ax    
    ja  _get_out    
    mov bx, word ptr [si+bp]	;bx = frequency[current_pos] 2nd set of bytes
    mov ax, word ptr [di]		;ax = total 2nd set of bytes
    cmp bx, ax
    jb  _huff_out_loop
    ;---------END EXIT CONDITION--------

	
_get_out:   	    
    mov  ax, last_point
    mov  root ax      
    push di
    push si
    push dx
    push cx
    push bx
    push ax     
;------------------HUFFMAN BODY--------------------------------
;This is the main body for the Huffman Function
;
;--------------------------------------------------------------                                                                                           


;------------------ENCODE--------------------------------
;Encode the bits
;
;--------------------------------------------------------     
_encode:
    mov  si, offset message     
    sub  bp, bp   
    sub  cx, cx  
    sub  bx, bx
    sub  ax, ax  
    ;Reset pointer
    xor  ax, ax
    mov  ah, 42h
    mov  al, 0
    mov  bx, open_h
    mov  dx, 0
    int  21h        
    jmp  _encode_l
    
	
_open_file_err:
    xor  ax, ax
    mov  ah, 09h
    mov  dx, offset huffman_msg
    int  21h
    jmp  exit
    
    
_encode_l:        
    xor  ax, ax
    mov  ah, 3Fh
    mov  bx, open_h
    mov  cx, 1
    mov  dx, offset message_r
    int  21h                   
    jc   _open_file_err
    cmp  ax, cx                    ;EOF
    jne  _done_encode
    mov  ax, message_r     ;l and i
    xor  ah, ah
    mov  bx, 4
    mul  bx    
    cmp  ax, 0   
    mov  i , ax
    mov  cx, 0                    ;count
    
	
_encode_inl:
    xor  dx, dx
    mov  dx, i   
    mov  ax, root
    cmp  dx, ax 
    jae  _encode_byte  
    ;move i into old i  
    mov  old_i, dx        
    mov  di, offset parent
    mov  bx, i
    xor  dx, dx
    mov  dx, [di+bx]
    mov  i , dx  
    mov  di, offset upkid
    mov  bx, i
    mov  ax, [di+bx] 
    mov  bx, old_i
    cmp  ax, bx
    mov  dx, encode_message_lnt   
    jne  _down_kid
    
    
_up_kid:         
    inc  dx
    mov  encode_message_lnt, dx
    push 1
    jmp  _encode_inl

	
_down_kid:                           
    inc  dx
    mov  encode_message_lnt, dx
    push 0
    jmp  _encode_inl
    
	
_encode_byte: 
    mov  cx, encode_message_lnt
    mov  di, offset encode_message     
    mov  bx, encode_message_ptr    
    
	
_encode_byte_l:
    pop  ax                  
    shr  al, 1
    rcl  bits, 1
    inc  bitcount
    cmp  bitcount, 8
    jnz  _done_byte
    call _write_byte
    mov  bitcount, 0  
	
	
_done_byte:
    ;mov byte ptr [di+bx], al
    inc  bx 
    loop _encode_byte_l      


_encode_byte_d:                
    mov  encode_message_lnt, 0
    mov  encode_message_ptr, bx
    inc  bp
    jmp  _encode_l
    
	
;------------------WRITE BYTE----------------------------------
;
;--------------------------------------------------------------      
_write_byte:
    push ax
    push cx
    push dx
    push bx    
    xor  al, al
    mov  ah, 40h
    mov  bx, out_h  
    mov  cx, 1
    mov  dx, offset bits
    int  21h    
    pop  bx
    pop  dx
    pop  cx
    pop  ax
    ret
;------------------WRITE BYTE----------------------------------
;
;-------------------------------------------------------------- 


_done_encode: 
    mov  ax, bitcount
    mov  endbitcount, ax 
    cmp  bitcount, 0
    jnz  _pad_byte
    jmp  _close_prog
	
	
_pad_byte:
    mov  al, 1
    shr  al, 1
    rcl  bits, 1    
    inc  bitcount
    cmp  bitcount, 8
    jnz  _pad_byte
    jmp  _close_prog      
    
	
_close_prog:
    call _write_byte  
    xor  ax, ax
    mov  al, byte ptr endbitCount
    mov  bits, al
    call _write_byte  
    mov  ax, open_h
    mov  close_h, ax
    call _close_file
    mov  ax, out_h
    mov  close_h, ax
    call _close_file 
    jmp  exit
     

;----------------CLOSE FILE----------------------------------
;
;------------------------------------------------------------
_close_file:
    push ax
    push cx
    push dx    
    mov  ah, 3eh
    mov  bx, close_h
    int  21h       
    jc   _close_file_error    
    pop  dx
    pop  cx
    pop  ax
    ret

	
_close_file_error:
    pop  dx
    pop  cx
    pop  ax
    ret       
;----------------CLOSE FILE----------------------------------
;
;------------------------------------------------------------    
    
	
;------------------ENCODE--------------------------------
;Encode the bits
;
;--------------------------------------------------------   
exit:
.exit                 
ret

