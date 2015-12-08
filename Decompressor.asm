;You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt

org 100h
jmp start        

    ;fileToDecode      db 'encode.dat',0 
    ;fileToEncode      db 'test2.txt',0 
    create       db 'testout.txt' 
      
     
    fileToEncode db 256 dup(?)
    fileToDecode db 256 dup(?)
     
    ;File Handles
    outH         dw ?    ;Create File Handle
    openH        dw ?    ;Open File Handle
    closeH       dw ?    ;Close File Handle

                      
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
    messageR     db ? 
    messageB     db ? 
    
    ;Encoded message -- message to be encoded
    encodeMessage    dw 1024 dup(?) 
    encodeMessageptr dw 0  
    encodeMessagelnt dw 0               
    
    ;Encode the input
    bits         db ?
    bitcount     db ?  
    endbitcount  dw ?    
         
    ParseMessageError  dw "Error with command. Unrecognized instructions. $"
    CreateFileMsgError dw "Error with creating file. Must be under 8 chars!. $"
    huffmanmsg         dw "Error opening file. $"
    
    minCount     dd 0   
    total        dd 0            ;BYTE LIMITATION IS 65536. TOTAL CANNOT BE MORE!
    value_min_p1 dd 0            ;Value of what's in position 1
    value_min_p2 dd 0            ;Value of what's in position 2    
    min_count_t  dd 0       
    
    word_segment dw ?            ;These indicate the offset and the segments
    word_offset  dw ?       
                          
    errorMEssage dw 'Error at: ',0
    
    ;This is for decode
    first_byte   db ? 
    second_byte  db ?   
    
    ;Pos tree
    pos          dw ?
    pos_tree     dw ?
    
    
start:   
      
    call ParseCommandFile
    
    call OpenFile
                                 
    call CreateFile
     
    call INITIALIZE 
    
    call TOTALFREQ 
    call HUFFMAN_BODY                                             
    call Decode
    
    jmp exit  
    
;----------------PARSING COMMAND LINE------------------------
;
;------------------------------------------------------------
  
ParseCommandFile:
    push ax
    push bx
    push cx
    push dx 
    push si 
    push di
    
    mov si, 81h
    mov di, offset fileToDecode 
L1: 
    xor ax, ax
    mov al, byte ptr [si] 
    inc si
    cmp al, 0Dh
    je errorParseFile
    cmp al, 20h ;Space 
    je L1  

L2:                                         ;Input file                
    mov byte ptr [di], al
    inc di                                                             
    mov al, byte ptr [si]                                              
    inc si
    cmp al, 0dh  
    je errorParseFile
    cmp al, 20h 
    jne L2
    
    inc di
    mov byte ptr [di], 0                    ;Null terminated string
    mov di, offset fileToEncode   
     
    
L3:
    mov al, byte ptr [si]
    inc si 
    cmp al, 0dh
    je  endParse
    mov byte ptr [di], al
    inc di      
    jmp L3
    
    
endParse:  
    inc di
    mov byte ptr [di], 0 
       
    pop di   
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret   
 
errorParseFile:  
    xor ax, ax
    mov ah, 9h
    mov dx, offset ParseMessageError
    int 21h
    jmp exit


;----------------PARSING COMMAND LINE------------------------
;
;------------------------------------------------------------
                                       

;----------------OPEN FILE-----------------------------------
;
;------------------------------------------------------------
OpenFile:

    push ax
    push cx
    push dx
    
    mov ah, 3Dh
    mov al, 0h
    mov dx, offset fileToDecode
    sub cx, cx
    int 21h
    jc  OpenFileError
    mov openH, ax 
    
    pop ax
    pop cx
    pop dx
    ret

OpenFileError:
    pop dx
    pop cx
    pop ax 
    ret   
                                               
;----------------OPEN FILE-----------------------------------
;
;------------------------------------------------------------
       
     
     
                         
;------------------CREATE FILE---------------------------------
;
;--------------------------------------------------------------    
CreateFile: 
    
    push ax
    push cx
    push dx
    
    mov ah, 3ch
    sub cx, cx
    mov dx, offset fileToEncode
    int 21h
    jc  CreateFileError
    mov outh, ax
    
    pop ax
    pop cx
    pop dx
    ret

CreateFileError:  
    xor ax, ax   
    xor dx, dx
    mov ah, 9h
    mov dx, offset CreateFileMsgError
    int 21h
    jmp exit


;------------------CREATE FILE---------------------------------
;
;--------------------------------------------------------------

;------------------WRITE TABLE----------------------------------
;
;--------------------------------------------------------------  
WRITETABLE:             
    push ax
    push cx
    push dx
    push bx
    
    mov  ah, 40h
    mov  bx, outh  
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
       



;------------------INITIALIZE FREQUENCY TABLE------------------    
;This method initializes the frequency table. Counts the 
;presence of each symbol encountered.
;--------------------------------------------------------------
INITIALIZE:        
    push bx
    push di
    push bp
    push ax
    push dx 
    push si
    
    mov  di,     offset frequency	;Set di to the position of frequency 
    ;mov  si,     offset mincount	;Set si to the position of mincount        
     
    xor  ax, 	 ax			                ;0 out ax register
    mov  ah,     3Fh			            ;Instruction to read file
    mov  bx, 	 openH		  	            ;Give it the file to be read file handle
    mov  cx, 	 2048   	                ;cx = 1 number of bytes to be read
    mov  dx,     di     	        ;Store result in messageR
    int  21h
    
    pop  si
    pop  dx
    pop  ax
    pop  bp
    pop  di
    pop  bx
    ret          
;------------------INITIALIZE FREQUENCY TABLE------------------    
;This method initializes the frequency table. Counts the 
;presence of each symbol encountered.
;--------------------------------------------------------------
 

;------------------COUNT TOTAL FREQUENCY-----------------------
;Goes through the list and finds the TOTAL FREQUENCY
; MAXES OUT AT AROUND 4294967296 BYTES OR 4 MB
;-------------------------------------------------------------- 
TOTALFREQ:    
    push ax
    push bx
    push si
    push di
    push cx 
                    
    
    mov  si, 	 offset frequency       ;Set si to the position of frequency
    mov  di,     offset total           ;Set di to the position of total 
    mov  cx,     256                   	;Size of the frequency table

    
TFL: 
    					;This sets the first Word of the 4 byte word
    mov  ax, 	 word ptr total 	;Set ax to value of total
    mov  bx,	 word ptr [si]    	;Set bx to value of SI
    add  ax,     bx  	
    mov  total,  ax            		;Add them together    
    
    
    jnc  LTHAN65535C			;Carry has been detected 
    inc  word ptr [di+2]	        ;increment whatever is in total by 1

LTHAN65535C:                                        					
    mov  ax, 	 word ptr [di+2]	;Set ax to the value of whatever is in total
    mov  bx, 	 word ptr [si+2]   	;Set bx to the value of whatever is in second word in frequency table
    add  ax, 	 bx
    mov  [di+2], ax     
    
    
    add  si,	 4                     	;Increment si by 4...frequency table is spaced by double words
    loop TFL       
    
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
HUFFMAN_BODY: 

    
    
    push ax
    push bx
    push cx
    push dx
    push si
    push di    
    
    
    sub  bx,     bx       
    
    mov  si,     offset total                                                            
    mov  di,     offset mincount

    mov  bx,     word ptr [si]                   ;Set mincount equal to total
    mov  word ptr [di],   bx                   
    mov  bx, 	 word ptr [si+2]
    mov  word ptr [di+2], bx  
    
                

HUFFOUTLOOP:   
                            
    
    
    sub  bx, 	 bx
    mov  si, 	 offset mincount
    mov  di, 	 offset min_count_t
     
    mov  bx, 	 word ptr [si]			 ;Set value of min_count_temp = min_count
    mov  word ptr [di],bx  
    mov  bx, 	 word ptr [si+2]
    mov  word ptr [di+2], bx
    
    mov  si, 	 offset value_min_p1		 ;Set value of value_min_p1 = 0
    mov  word ptr [si], 0
    mov  word ptr [si+2], 0

    mov  si, 	 offset value_min_p2		 ;Set value of value_min_p2 = 0
    mov  word ptr [si], 0       
    mov  word ptr [si+2], 0 
      
    ;----------FIND MIN POS 1 ROUTINE------------------------
    ;Find the position of the first minimum and it's value
    ;-------------------------------------------------------- 
    xor  dx,	 dx
    xor  cx,	 cx
    mov  ax,	 root
    mov  bx,	 4
    div  bx
    mov  cx,	 ax		                 ;Loop condition. Root/4. Starts at 256 

    xor  si,	 si
    mov  si, 	 offset frequency  		 ;si = frequency[pos]  
    mov  di,     offset min_count_t  		 ;di = min_count_t pos 
    xor  ax,     ax

MINPOS1:     
    
    sub  bx, 	 bx   				 ;word_segment = min_count_t (1st 2 bytes)
    mov  bp,	 ax       
    mov  bx, 	 word ptr [di+2]
    mov  word_segment, bx			 
    
    mov  bx, 	 word ptr [di] 			 ;word_offset  = min_count_t (2nd 2 bytes)
    mov  word_offset,  bx 
 
    mov  dx, 	 word ptr [si+bp+2]		                  
    cmp  dx, 	 0
    jz   C1					 ;frequency[pos+2] == 0 check frequency[pos] (1st 2 bytes)
    cmp  dx, 	 word_segment
    jb 	 HUFFC1					 ;frequency[pos+2] <  min_count_t (1st 2 bytes) Change min_count values
    cmp  dx,	 word_segment
    ja   C2					 ;frequency[pos+2] >  min_count_t (1st 2 bytes) Loop again       
    
C1:
    mov  dx, 	 word ptr [si+bp]		                 
    cmp  dx, 	 0				 
    jz   C2           				 ;frequency[pos] == 0 check frequency[pos] (2nd 2 bytes) Loop again  
    cmp  dx,	 word_offset
    jb   HUFFC1					 ;frequency[pos] <  min_count_t (2nd 2 bytes) Change min_count values

C2:
    add  ax, 	 4
    LOOP MINPOS1 
    jmp  DMIN1    
      
HUFFC1:    
    
    mov  dx, 	 word ptr [si+bp]		 ;min_count_t 2nd bytes = frequency[pos]
    mov  word ptr [di], 	 dx   
    
    mov  dx,	 word ptr [si+bp+2]  		 ;min_count_t 1st bytes = frequency[pos+2]		
    mov  word ptr [di+2], dx
    
    mov  min_pos1, ax     
    add  ax, 	 4
    LOOP MINPOS1 
    ;----------FIND MIN POS 1 ROUTINE------------------------
    ;Loop Back
    ;--------------------------------------------------------


DMIN1:    
    
    mov  di,     offset value_min_p1    					
    sub  bp, 	 bp 				 ;Position of the first minimal                
    mov  bp, 	 min_pos1    
    sub  ax, 	 ax            
                     
    mov  ax, 	 word ptr [si+bp] 			 ;Value_min_p1 = frequency[pos]         
    mov  word ptr [di], ax
    mov  ax, 	 word ptr [si+bp+2]          
    mov  word ptr [di+2], ax    
        
    mov word ptr [si+bp], 0			 ;Clear the Values at frequency[min_pos1]
    mov word ptr [si+bp+2], 0

    mov di, 	 offset mincount                     
    mov bx, 	 word ptr [di]			 ;Set value of min_count_temp = min_count
    mov di,      offset min_count_t
    mov word ptr [di],bx 

    mov di,	 offset mincount
    mov bx, 	 word ptr [di + 2]
    mov di,	 offset min_count_t
    mov word ptr [di+2], bx               
    
    
    ;----------FIND MIN POS 2 ROUTINE------------------------
    ;Find the position of the second minimum and it's value
    ;--------------------------------------------------------   
    
    xor  dx,	 dx
    xor  cx,	 cx
    mov  ax,	 root
    mov  bx,	 4
    div  bx
    mov  cx,	 ax

    xor  si,	 si
    mov  si, 	 offset frequency  		 ;si = frequency[pos]  
    mov  di,     offset min_count_t  		 ;di = min_count_t pos         
    xor  ax,     ax

MINPOS2:    
    sub  bx, 	 bx   				 ;word_segment = min_count_t (1st 2 bytes)
    mov  bp,	 ax       
    mov  bx, 	 word ptr [di+2]
    mov  word_segment, bx			 
    
    mov  bx, 	 word ptr [di] 			 ;word_offset  = min_count_t (2nd 2 bytes)
    mov  word_offset,  bx 
 
    mov  dx, 	 word ptr [si+bp+2]		                  
    cmp  dx, 	 0
    jz   C12					 ;frequency[pos+2] == 0 check frequency[pos] (1st 2 bytes)
    cmp  dx, 	 word_segment
    jb 	 HUFFC2					 ;frequency[pos+2] <  min_count_t (1st 2 bytes) Change min_count values
    cmp  dx,	 word_segment
    ja   C22					 ;frequency[pos+2] >  min_count_t (1st 2 bytes) Loop again

C12:
    mov  dx, 	 word ptr [si+bp]		                 
    cmp  dx, 	 0				 
    jz   C22           				 ;frequency[pos] == 0 check frequency[pos] (2nd 2 bytes) Loop again  
    cmp  dx,	 word_offset
    jb   HUFFC2					 ;frequency[pos] <  min_count_t (2nd 2 bytes) Change min_count values

C22:
    add  ax, 	 4
    LOOP MINPOS2 
    jmp  DMIN2     
    
HUFFC2: 
    mov  dx, 	 word ptr [si+bp]		 ;min_count_t 2nd bytes = frequency[pos]
    mov  word ptr [di], 	 dx   
    
    mov  dx,	 word ptr [si+bp+2]  		 ;min_count_t 1st bytes = frequency[pos+2]		
    mov  word ptr [di+2], dx
    
    mov  min_pos2, ax     
    add  ax, 	 4
    LOOP MINPOS2 
    ;----------FIND MIN POS 2 ROUTINE------------------------
    ;Loop Back
    ;--------------------------------------------------------  

DMIN2:   
    mov  di,     offset value_min_p2    					
    sub  bp, 	 bp 				 ;Position of the first minimal                
    mov  bp, 	 min_pos2    
    sub  ax, 	 ax            
                     
    mov  ax, 	 word ptr [si+bp] 			 ;Value_min_p2 = frequency[pos]         
    mov  word ptr [di], ax
    mov  ax, 	 word ptr [si+bp+2]          
    mov  word ptr [di+2], ax    
        
    mov  word ptr [si+bp], 0			 ;Clear the Values at frequency[min_pos2]
    mov  word ptr [si+bp+2], 0
  
                                  
    sub  bp, 	 bp                                                                    
    mov  bp, 	 root                                     
    
   ;---------upkid-------------------
    mov  si,	 offset upkid 			 ;Set upkid[pos] = min_pos_1 (position of first minimal)
    sub  bx,	 bx
    mov  bx,	 min_pos1            
    mov  word ptr [si+bp], bx                   
    ;---------upkid-------------------
    
    ;---------downkid-----------------
    mov  si,	 offset downkid 		 ;Set downkid[pos] = min_pos_2 (position of second minimal) 
    sub  bx, 	 bx
    mov  bx, 	 min_pos2
    mov  word ptr [si+bp], bx                   
    ;---------downkid-----------------   
    
    ;---------frequency root----------
    mov  si, 	 offset frequency 
    
    mov  di, 	 offset value_min_p1		 ;Add value_min_p1 and value_min_p2 and get the total (1st bytes)
    sub  bx,	 bx
    mov  bx, 	 word ptr [di]   

    mov  di, 	 offset value_min_p2
    sub  ax,     ax
    mov  ax,	 word ptr [di]
    
    add  ax,	 bx
    jnc  LTHAN65535CF 				 ;If carry exists, increment 1st set of bytes
    mov  bx,     word ptr [si+bp+2]
    add  bx,     1
    mov  word ptr [si+bp+2], bx
      
LTHAN65535CF:
    mov  word ptr[si+bp], ax

    mov  di, 	 offset value_min_p1		 ;bx = value_min_p1 high order
    sub  bx,	 bx
    mov  bx, 	 word ptr [di+2]   

    mov  di, 	 offset value_min_p2         ;ax = value_min_p2 high order
    sub  ax,     ax
    mov  ax,	 word ptr [di+2]

    add  ax,	 bx                          ;ax = ax + bx                
    
    mov  bx,     word ptr [si+bp+2]            ;bx = frequency[pos+2]            
    add  ax,     bx                          ;ax = ax + bx


    mov  word ptr [si+bp+2], ax              ;frequency[pos+2] = ax               
    ;---------frequency root----------
    
    sub  bx, 	 bx
    mov  bx, 	 root
    ;---------change parent 1/2---------                                  
    mov  si, 	 offset parent 

    sub  bp, 	 bp				 ;Parent of min_pos1
    mov  bp, 	 min_pos1
    mov  word ptr [si+bp], bx

    sub  bp,	 bp				 ;Parent of min_pos2
    mov  bp,	 min_pos2
    mov  word ptr [si+bp], bx
    ;---------change parent 1/2---------                                  
    
    mov  last_point, bx
    add  bx, 	 4
    mov  root,	 bx
    
    mov si, 	 offset frequency
    mov di,	     offset total

    mov bp, 	 last_point
    mov bx, 	 word ptr [si+bp+2]			 ;bx = frequency[current_pos] 1st set of bytes
    mov ax, 	 word ptr [di+2]				 ;ax = total 1st set of bytes
    cmp bx, 	 ax					 
    
    
    jb  HUFFOUTLOOP				 ;frequency[current_pos]<total 1st set of bytes Re-Loop 
    cmp bx,      ax    
    
    ja  GETOUT   
    
    
    mov bx, 	 word ptr [si+bp]			 ;bx = frequency[current_pos] 2nd set of bytes
    mov ax, 	 word ptr [di]				 ;ax = total 2nd set of bytes
    cmp bx, 	 ax
    jb  HUFFOUTLOOP
    ;---------END EXIT CONDITION--------

GETOUT:   	    
    
    mov ax, 	 last_point
    mov root, 	 ax  
            
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

;------------------Decode--------------------------------
;Decode the bits
;
;--------------------------------------------------------   

    
                                
Decode:
    mov si, offset message     
    
    sub bp, bp   
    sub cx, cx  
    sub bx, bx
    sub ax, ax     

    ;Pos_tree
    mov bx, root
    mov pos_tree, bx
             
    
    jmp DecodeL
    
OpenFileErr:
    xor ax, ax
    mov ah, 09h
    mov dx, offset huffmanmsg
    int 21h
    jmp exit
    
     
    
DecodeL:        
    
    xor ax, ax
    mov ah, 3Fh
    mov bx, openH
    mov cx, 1
    mov dx, offset messageB
    int 21h                   
    jc  OpenFileErr
    cmp ax, cx                    ;EOF
    jne DONEDECODE      
        
CHKFIRST:                ;if null
    mov dl, first_byte
    cmp dl, 0       
    jne  CHKSECOND                ;if null
    mov dl, messageB
    mov first_byte, dl
    jmp DecodeL
    
CHKSECOND:
    mov dl, second_byte
    cmp dl, 0
    jne PROCESSB
    mov dl, messageB
    mov second_byte, dl
    jmp DecodeL
    
PROCESSB:
        
    Call DecodeByte                 ;Decode byte
        
    xor ax, ax
    mov al, second_byte             ;first_byte = second_byte
    mov first_byte, al                                       
    
    mov al, messageB
    mov second_byte, al             ;second_byte = byte_read   
    
    jmp DecodeL
    
    
DecodeByte:
    push ax
    push bx
    push cx
    push dx      
             
    
    ;Pos
    mov ax, 0
    mov pos, ax 
   
    xor bx, bx
    mov bl, 8
    mov bitcount, bl 
    mov al, bitcount
    
        
DecodeTree:
   
    cmp al, 0     
    jne contDec
    jmp finDec 
    
contDec:       
    sub al, 1
    mov bitcount, al
    mov al, first_byte       
    shl al, 1
    mov first_byte, al 
    jnc poseq0

poseq1:
    ;pos-tree = p
    mov si, offset upkid
    mov bx, pos_tree
    mov ax, word ptr [si+bx]
    mov pos_tree, ax  
    jmp chkLRoot


poseq0:
    mov si, offset downkid
    mov bx, pos_tree
    mov ax, word ptr [si+bx]
    mov pos_tree, ax

chkLRoot:
    xor ax, ax
    mov al, bitcount
    mov bx, pos_tree
    cmp bx, 400h     
    jae DecodeTree 
    
    mov ax, bx
    xor dx, dx 
    xor bx, bx
    mov bx, 4
    div bx    
    mov messageR, al   
    
    Call WRITEBYTE
    mov bx, root
    mov pos_tree, bx
    
    xor ax, ax
    mov al, bitcount
    jmp DecodeTree
            
   
finDec:
    pop dx
    pop cx
    pop bx
    pop ax
    ret    
    
DecodeBitDone:
        
    pop  dx
    pop  cx
    pop  bx
    pop  ax
    ret    
              
              

    
;------------------WRITE BYTE----------------------------------
;
;--------------------------------------------------------------      
WRITEBYTE:
    push ax
    push cx
    push dx
    push bx
    
    xor  al, al
    mov  ah, 40h
    mov  bx, outh  
    mov  cx, 1
    mov  dx, offset messageR
    int  21h
    
    pop  bx
    pop  dx
    pop  cx
    pop  ax
    ret
;------------------WRITE BYTE----------------------------------
;
;-------------------------------------------------------------- 

DONEDECODE:  
    push ax
    push bx
    push cx
    push dx
    
    xor ax, ax
    mov al, second_byte
    mov bitcount, al
    call DecodeTree

;----------------CLOSE FILE----------------------------------
;
;------------------------------------------------------------
CloseFile:
    push ax
    push cx
    push dx
    
    mov ah, 3eh
    mov bx, closeH
    int 21h       
    jc  CloseFileError
    
    pop dx
    pop cx
    pop ax
    ret

CloseFileError:
    pop dx
    pop cx
    pop ax
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