.model small
.stack 200h
.data                                            
    duom db 255 dup (0)
    rez db 255 dup (0)
    er1 db "Nepavyko atidaryti failo$"
    er2 db "Ivyko skaitymo klaida$"
    help db "Disasembleri parase Matas Savickis$"
    nuliai db "ADD $"
    astuonetai db "MOV MOV POP ADD SUB CMP JO  $"
    w0 db "alcldlblahchdhbh$"
    w1 db "axcxdxbxspbpsidi$"
    mod00opr db "[]bx+sibx+dibp+sibp+di  si   di        bx $"
    mod12opr db "[]bx+si+    bx+di+    bp+si+    bp+di+    si+       di+       bp+       bx+       $"
    penkiolikai db "INC DEC MUL DIV CALLCALLJMP JMP PUSH$"
    septynetai db "JO  JNO JNAEJAE JE  JNE JBE JAE JS  JNS JP  JNP JL  JGE JLE JG  $"
    trejetai db "CMP CMP XOR XOR$"
    penketai db "PUSHPOP $"
    dvejetai db "SUB SUB $"
    dvyliktukai db "MOV RET RET RETFRETFINT $"
    ketvertai db "INC DEC $"
    keturioliktukai db "JMP JMP JMP CALLJCXZLOOP$"
    astuntukai db "MOV MOV POP ADD SUB CMP$"
    na db "Neatpazinta$"
    ptrtxt db "byte ptrword ptr$"
    segregai db "escsssds$"
    kitie db "PUSHPOP CALL$"
    desk dw 00, 00
    skbuf db 16 dup (?)
    rezbuf db 255 dup (' ')
    rezbuft db 255 dup (' ')
    poslinkis db 0,1,0,0
    poslinkisraw dw 100h
    failopozicija dw 0
    poslinkisisproceduros db 0
    enter db 13, 10
    srn dw 0
    srnr dw 0 
.code
origin:
    mov     dx, @data
    mov     ds, dx 
    
    call parametrai 
    call open
    
begin:   
    call cleanrez
    mov dx, 0000h
    mov dl, poslinkisisproceduros
    call poslinkiskode
    mov srn, 0
    mov srnr, 0
    mov rezbuf[5], ' '
    mov rezbuf[4], ':'
    mov rezbuf[66], ','
   
    call skaityti
   ;mov skbuf[0], 10100000b ;A20E02 
   ;mov skbuf[1], 00001110b
   ;mov skbuf[2],  00000010b
    ;mov skbuf[3], 00000010b
    ;mov skbuf[4], 00100011b
   esreg: 
    cmp skbuf[0],26h
    jne csreg
    mov srn, 1
    mov srnr, 0
    call srskaitymas
    mov rezbuf[8], '2'
    mov rezbuf[9], '6' 
    
   csreg: 
    cmp skbuf[0], 2Eh
    jne ssreg
    mov srn, 1
    mov srnr, 2
    call srskaitymas
    mov rezbuf[8], '2'
    mov rezbuf[9], 'E'
    
   ssreg:
    cmp skbuf[0], 36h
    jne dsreg
    mov srn, 1
    mov srnr, 4
    call srskaitymas
    mov rezbuf[8], '3'
    mov rezbuf[9], '6'
    
   dsreg:
    cmp skbuf[0], 3Eh
    jne nesegreg
    mov srn, 1
    mov srnr, 6
    call srskaitymas
    mov rezbuf[6], '3'
    mov rezbuf[6], 'E'
    
       
    
nesegreg:
     
    call astuoni
    call vienuolika
    call kitokie
    call keturiolika
    call desimt
    call dvylika
    call trys
    call du
    call keturi
    call penki
    call nulis
    call penkiolika 
    call septyni
    
    
    call neatpazinta
    jmp begin
    
    jmp exit
    
    neastuonijmp:
        jmp neastuoni
    
    proc srskaitymas
        mov ah, 42h
        mov al, 0
        mov bx, desk[0]
        mov poslinkisisproceduros, 1
        mov dx, failopozicija
        add dl, poslinkisisproceduros
        mov cx, 0
        mov failopozicija, dx
        int 21h
        mov poslinkisisproceduros, 0
        mov bx, desk[0]
        mov cx, 7h
        mov dx, offset skbuf
        mov ah, 3Fh
        int 21h
        cmp ax, 0
        je exitsr

    ret
endp srskaitymas    
 exitsr:
	jmp exit
   
   
   
    proc astuoni
    mov al, 11110000b
    and al, skbuf[0]
    cmp al, 10000000b
    jne neastuonijmp
    mov al, 00001100b
    and al, skbuf[0]
    _1000_10dw:
    cmp al, 00001000b
    jne _1000_10d0
    mov di, 0d
    call astuonikodas
    call reg
    call modrm
    mov cx, 0
    call lseek
    mov cx,0
    mov cl, poslinkisisproceduros
    call naudojamaskodas
    call spausdinti
    jmp begin

    _1000_1111jmp:
    jmp _1000_1111
    
    _1000_10d0:
    mov al, 00001101b
    and al, skbuf[0]
    cmp al, 00001100b
    jne _1000_1111jmp
    mov di, 4d
    call astuonikodas
    call modrm
    mov al, 00000010b
    and al, skbuf[0]
    cmp al, 0
    jne _10d0_1
    mov cx, 0
    mov cl, 00011000b
    and cl, skbuf[1]
    ror cl, 3
    
    mov di, 45h
    mov si, 0
    inc cl
    _10d0:
    mov al, segregai[si]
    mov rezbuf[di], al
    mov al, segregai[si+1]
    mov rezbuf[di+1], al
    add si, 2
    
    
    loop _10d0
     mov cx, 0
    call lseek
    mov cx,0
    mov cl, poslinkisisproceduros
    call naudojamaskodas
    call spausdinti
    jmp begin   
    
    
    
    _10d0_1:
    mov cx, 0
    mov cl, 00011000b
    and cl, skbuf[1]
    ror cl, 3
   
    mov di, 25h
    mov si, 0
    inc cl
    _10d0_1loop:
    mov al, segregai[si]
    mov rezbuf[di], al
    mov al, segregai[si+1]
    mov rezbuf[di+1], al
    add si, 2
    
    
    loop _10d0_1loop
     mov cx, 0
    call lseek
    mov cx,0
    mov cl, poslinkisisproceduros
    call naudojamaskodas
    call spausdinti
    jmp begin
    
    _1000_1111:
    mov al, 00001111b
    and al, skbuf[0]
    cmp al, 00001111b
    jne _1000_000
    
    mov poslinkisisproceduros, 2
    call reg
    mov di, 8d
    call astuonikodas
    mov cx, 0
    call lseek
    mov cx,0
    mov cl, poslinkisisproceduros
    call naudojamaskodas
    call spausdinti
    jmp begin
    _1000_000:
    mov al, 00111000b
    and al, skbuf[1]
    cmp al, 0
    jne _1000_101 
    mov di, 12d
    call astuonikodas
    jmp _8ptrkodas
    call padaromtapaskutini
    
    jmp begin
    _1000_101:
    mov al, 00111000b
    and al, skbuf[1]
    cmp al, 00101000b
    jne _1000_111 
    mov di, 16d
    call astuonikodas
    call padaromtapaskutini
    jmp _8ptrkodas
    jmp begin
    
    _1000_111:
    mov al, 00111000b
    and al, skbuf[1]
    cmp al, 00111000b
    jne _1000_111 
    mov di, 20d
    call astuonikodas
    call padaromtapaskutini
    jmp begin
    
    _8ptrkodas:
            mov al, skbuf[0]
            and al, 00000001h
            cmp al, 0
            jne _8wordptr
            mov di, 0d
            call ptrwkodas
            call padaromtapaskutini
            jmp begin
            
            _8wordptr:
            mov di, 8d
            call ptrwkodas
            call padaromtapaskutini
            jmp begin
     
    
    
    
        
    
    
    neastuoni:
    ret
endp astuoni
    
    proc padaromtapaskutini
        mov dx, 0
        mov dl, skbuf[0]
        push dx
        and skbuf[0], 00000001b
        call modrm
        mov di, 25h
        ;0mov rezbuf[di], '['
        ;mov rezbuf[di+11], ']'
        pop dx
        mov skbuf[0], dl
        mov al, 00000001b 
        and al, skbuf[0]
        cmp al, 0
        jne wpask1
        
        wpask0:
            mov ax, 0
            mov al, poslinkisisproceduros
            mov si, ax
            inc poslinkisisproceduros
            mov dl, skbuf[si]
            call makemetwochar
            mov rezbuf[68], dh
            mov rezbuf[69], dl 
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin 
        
        
        
        
        
        
        
        tesiampaskutini:
           
        
        
        
        wpask1: 
          w1s0:
          mov al, 00000010b
          and al, skbuf[0]
          cmp al, 0
          jne w1s1
          mov ax, 0
          mov al, poslinkisisproceduros
          mov si, ax
          add poslinkisisproceduros, 2
          mov dl, skbuf[si+1]
          call makemetwochar
          mov rezbuf[68], dh
          mov rezbuf[69], dl
          mov dl, skbuf[si]
          call makemetwochar
          mov rezbuf[70], dh
          mov rezbuf[71], dl
          mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
          
             
          
          
          
          
          
          
          w1s1:
          mov ax, 0
          mov al, poslinkisisproceduros
          mov si, ax
          inc poslinkisisproceduros
          cmp skbuf[si], 80h
          jle pletinysSff
          mov rezbuf[68], '0'
          mov rezbuf[69], '0'
          mov dl, skbuf[si]
          call makemetwochar
          mov rezbuf[70], dh
          mov rezbuf[71], dl
          mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
          
          
          pletinysSff:
          mov rezbuf[68], 'F'
          mov rezbuf[69], 'F'
          mov dl, skbuf[si]
          call makemetwochar
          mov rezbuf[70], dh
          mov rezbuf[71], dl
          mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        
        
        
    
    
        
    
    ret
endp padaromtapaskutini
    
    proc astuonikodas 
        mov cx, 0004h
        mov si, 20h
        astuonikodasloop:
        mov al, astuonetai[di]
        mov rezbuf[si], al
        inc si
        inc di
        loop astuonikodasloop    
    ret
endp astuonikodas
    
    nevienuolikajmp:
        jmp nevienuolika
    
    proc vienuolika
    mov al, 11110000b
    and al, skbuf[0]
    cmp al, 10110000b
    jne nevienuolikajmp
    mov di, 20h
    mov rezbuf[di], 'M'
    mov rezbuf[di+1], '0'
    mov rezbuf[di+2], 'V'
    mov al, 00001000b
    and al, skbuf[0]
    cmp al, 00001000b
    je vienuolikaw1
    vienuolikaw0:
    mov cx, 0
    mov cl, 00000111b
    and cl, skbuf[0]
    mov si, 0
    mov di, 25h
    inc cx
    _11W0:
    mov al, w0[si]
    mov rezbuf[di], al
    mov al, w0[si+1]
    mov rezbuf[di+1], al
    add si, 2
      
    
    loop _11W0
    
    mov dl, skbuf[1]
    call makemetwochar
    mov si, 45h
    mov rezbuf[si], dh
    mov rezbuf[si+1], dl
    mov poslinkisisproceduros, 2
    
    
    jmp vienuolikosveiksmai
    
    vienuolikaw1:
    mov cx, 0
    mov cl, 00000111b
    and cl, skbuf[0]
    mov si, 0
    mov di, 25h
    inc cx
    _11W1:
    mov al, w1[si]
    mov rezbuf[di], al
    mov al, w1[si+1]
    mov rezbuf[di+1], al
    add si, 2
      
    
    loop _11W1
    
    mov dl, skbuf[1]
    call makemetwochar
    mov si, 45h
    mov rezbuf[si+2], dh
    mov rezbuf[si+3], dl
    
    
    mov dl, skbuf[2]
    call makemetwochar
    
    mov rezbuf[si], dh
    mov rezbuf[si+1], dl
    mov poslinkisisproceduros, 3
    jmp vienuolikosveiksmai
    
    
    
    
    vienuolikosveiksmai:
    mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin 
       
    
    nevienuolika:
    ret 
endp vienuolika
    
    proc kitokie
        mov al, 11100111b
        and al, skbuf[0]
        
        _000sreg110:
        cmp al, 00000110b
        jne _000sreg111
        mov rezbuf[66], ' '
        mov si, 0d
        call kitokiukodas
        mov cl, 00011000b
        and cl, skbuf[0]
        ror cl, 3
        call segregkitoks
        mov poslinkisisproceduros, 1
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        _000sreg111:
         cmp al, 00000111b
         jne _10011010 
         mov rezbuf[66], ' '
         mov si, 4d
        call kitokiukodas
        mov cl, 00011000b
        and cl, skbuf[0]
        ror cl, 3
        call segregkitoks
        mov poslinkisisproceduros, 1
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        _10011010:
        cmp skbuf[0], 10011010b
        jne nekitoks
        mov si, 20h
        mov rezbuf[si], 'C'
        mov rezbuf[si+1], 'A'
        mov rezbuf[si+2], 'L'
        mov rezbuf[si+3], 'L'
        mov poslinkisisproceduros, 5
        
        mov rezbuf[si+5], '['
        mov dl, skbuf[4]
        call makemetwochar
        mov rezbuf[si+6], dh
        mov rezbuf[si+7], dl
        mov dl, skbuf[3]
        call makemetwochar
        mov rezbuf[si+8], dh
        mov rezbuf[si+9], dl
        mov rezbuf[si+10], ':'
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[si+11], dh
        mov rezbuf[si+12], dl
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[si+13], dh
        mov rezbuf[si+14], dl
        mov rezbuf[si+15], ']'
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        
        
        nekitoks:
        
    ret
endp kitokie
    
    proc segregkitoks 
        mov si, 0
        mov di, 25h
        inc cx
        segregkitokieloop:
        mov al, segregai[si]
        mov rezbuf[di], al
        mov al, segregai[si+1]
        mov rezbuf[di+1], al
        add si, 2
        
        
        
        loop segregkitokieloop
        
        
            
    
    ret
endp segregkitoks
    
    proc kitokiukodas
        mov cx, 0004h
        mov di, 20h
        kitoksloop:   
        mov al, kitie[si]
        mov rezbuf[di], al
        inc si
        inc di
        
        loop kitoksloop    
    
    ret
endp kitokiukodas
    
    
    neketuriolikajmp:
        jmp neketuriolika
    
    proc keturiolika
        mov al, 11110000b
        and al, skbuf[0]
        cmp al, 11100000b
        jne neketuriolikajmp
        mov rezbuf[66], ' '
        mov al, 00001111b
        and al, skbuf[0]
        
        _1110_1011:
        cmp al, 00001011b
        jne _1110_1001
        mov di, 0d
        call keturiolikakodas
        call vienobaitoposlinkis
        mov poslinkisisproceduros, 2
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin 
        
        _1110_1001:
        cmp al, 00001001b
        jne _1110_1010
        mov di, 4d
        call keturiolikakodas
        mov poslinkisisproceduros, 3
        mov dh, skbuf[2]
        mov dl, skbuf[1]
        add dx, 2
        add dx, poslinkisraw
        mov di, 25h
        push dx
        mov dh, 00
        call makemetwochar
        mov rezbuf[di], dh
        mov rezbuf[di+1], dl
        pop dx
        xchg dh,dl
        mov dh, 0
        call makemetwochar
        mov rezbuf[di+2], dh
        mov rezbuf[di+3], dl
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        
        _1110_1010:
        cmp al, 00001010b
        jne _1110_1000
        mov poslinkisisproceduros, 5
        mov di, 8d
        call keturiolikakodas
        mov di, 25h
        mov rezbuf[si], '['
        mov dl, skbuf[4]
        call makemetwochar
        mov rezbuf[si+1], dh
        mov rezbuf[si+2], dl
        mov dl, skbuf[3]
        call makemetwochar
        mov rezbuf[si+3], dh
        mov rezbuf[si+4], dl
        mov rezbuf[si+5], ':'
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[si+6], dh
        mov rezbuf[si+7], dl
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[si+8], dh
        mov rezbuf[si+9], dl
        mov rezbuf[si+10], ']'
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        
         
        _1110_1000:
        cmp al, 00001000b
        jne _1110_0011
        mov di, 12d
        call keturiolikakodas
        mov dl, skbuf[2]
        call makemetwochar
        mov di, 25h
        mov rezbuf[di], '['
        mov rezbuf[di+1], dh
        mov rezbuf[di+2], dl
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[di+3], dh
        mov rezbuf[di+4], dl
        mov rezbuf[di+5], ']'
        mov poslinkisisproceduros,3
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        
        _1110_0011:
        cmp al, 00000011b
        jne _1110_0010
        mov di, 16d
        call keturiolikakodas
        call vienobaitoposlinkis
        mov poslinkisisproceduros, 2
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        
        _1110_0010:
        cmp al, 00000010b
        jne neketuriolika
        mov di, 20d
        call keturiolikakodas
        call vienobaitoposlinkis
        mov poslinkisisproceduros, 2
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
        
                
        
        neketuriolika:
    ret
endp keturiolika
    
    proc keturiolikakodas
        mov cx, 0004h
        mov si, 20h 
        keturiolikakodasloop:
        mov al, keturioliktukai[di]
        mov rezbuf[si], al
        inc di
        inc si
        
        loop keturiolikakodasloop
            
    
    
    ret
endp keturiolikakodas
    
    nedesimtjmp:
        jmp nedesimt
    
    proc desimt
        mov al, 11110000b
        and al, skbuf[0]
        cmp al, 10100000b
        jne nedesimtjmp
        mov di, 20h
        mov rezbuf[di], 'M'
        mov rezbuf[di+1], 'O'
        mov rezbuf[di+2], 'V'
        mov poslinkisisproceduros, 3
        mov al, 00001110b
        and al, skbuf[0]
        cmp al, 0
        jne desimtantras
        desimtpirmas:
        call akumuliatorius
        mov dl, skbuf[1]
        call makemetwochar
        mov di, 40h
        mov rezbuf[di+5], ']'
        mov rezbuf[di+3], dh
        mov rezbuf[di+4], dl
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[di+1], dh
        mov rezbuf[di+2], dl 
        mov rezbuf[di], '['
         mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin    
        
        desimtantras:
        mov rezbuf[68], 'a'
        mov al, 00000001b
        and al, skbuf[0]
        cmp al, 0
        jne desimtw1
        mov rezbuf[69], 'l'
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[52], ']'
        mov rezbuf[50], dh
        mov rezbuf[51], dl
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[48], dh
        mov rezbuf[49], dl
        mov rezbuf[47], '['
         mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        
        desimtw1:
        mov di, 69d
        mov rezbuf[di], 'x'
         mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[35], dh
        mov rezbuf[36], dl
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[33], dh
        mov rezbuf[34], dl
         mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        nedesimt:     
    ret
endp desimt
    
    nedvylikajmp:
        jmp nedvylika
    
    _1100_0011jmp:
        jmp _1100_0011 
    
    proc dvylika
        mov al, 11110000b
        and al, skbuf[0]
        cmp al, 11000000b
        jne nedvylikajmp
        mov al, 00001110b
        and al, skbuf[0]
        _1100_011:
            
            cmp al, 00000110b
            jne _1100_0011jmp
            mov poslinkisisproceduros, 0
            mov al, skbuf[0]
            and al, 00000001h
            cmp al, 0
            jne _12wordptr
            mov di, 0d
            call ptrwkodas
            mov dl, skbuf[2]
            call makemetwochar
            mov rezbuf[69], dh
            mov rezbuf[70], dl
            jmp _12toliau
            
            _12wordptr:
            mov di, 8d
            call ptrwkodas
            mov dl, skbuf[2]
            call makemetwochar
            mov rezbuf[69], dh
            mov rezbuf[70], dl
            mov dl, skbuf[3]
            call makemetwochar
            mov rezbuf[67], dh
            mov rezbuf[68], dl
            
            _12toliau:
            mov si, 0d
            call dvylikakodas
            
            mov dl, skbuf[0]
            mov skbuf[0], 0h
            push dx
            call modrm
            pop dx
            mov skbuf[0], dl
            
            mov al, skbuf[0]
            and al, 00000001h
            cmp al, 0
            jne pridetidu
            inc poslinkisisproceduros
            jmp baigiambaliu
            pridetidu:
            add poslinkisisproceduros, 2h
            
            
            
            baigiambaliu:
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        _1100_0011:
            mov al, 00001111b
            and al, skbuf[0]
            cmp al, 00000011b
            jne _1100_0010
            mov rezbuf[66], ' '
            mov si, 4d
            call dvylikakodas
            mov poslinkisisproceduros, 1
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        _1100_0010: 
            cmp al, 00000010b
            jne _1100_1011
            mov rezbuf[66], ' '
            mov si, 8d
            call dvylikakodas
            mov poslinkisisproceduros, 3
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[di],dh
        mov rezbuf[di+1],dl
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[di+2],dh
        mov rezbuf[di+3],dl 
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        _1100_1011:
            cmp al, 00001011b
            jne _1100_1010
            mov rezbuf[66], ' '
            mov si, 12d
            call dvylikakodas
            mov poslinkisisproceduros, 1
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
         _1100_1010:
            cmp al, 00001010b
            jne _1100_1101
            mov rezbuf[66], ' '
            mov si, 16d
            call dvylikakodas
            mov poslinkisisproceduros, 3
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[di],dh
        mov rezbuf[di+1],dl
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[di+2],dh
        mov rezbuf[di+3],dl
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
          _1100_1101:
            cmp al, 00001101b
            jne nedvylika
            mov rezbuf[66], ' '
            mov si, 20d
            call dvylikakodas
            mov dl, skbuf[1]
            call makemetwochar
            mov di, 25h
            mov rezbuf[di], dh
            mov rezbuf[di+1], dl
            mov poslinkisisproceduros, 2 
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
            
        nedvylika:
    ret
endp dvylika
    netrysjmp:
	jmp netrys
    
    proc trys
        mov al, 11110000b
        and al, skbuf[0]
        cmp al, 00110000b
        jne netrysjmp
        mov al, 00001100b
        and al, skbuf[0]
        _0011_10:
            cmp al, 00001000b
            jne _0011_11 
            call tryskodas
            call reg
            call modrm
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        
        _0011_11:
            cmp al, 00001100b
	    jne _0011_00
	    
	    call tryskodas
            call akumuliatorius
            call bopr 
            call lseek
            
            mov cl, poslinkisisproceduros
           
            call naudojamaskodas
            call spausdinti
	    jmp begin
	 _0011_00:
		cmp al, 0
		jne _0011_01
		mov di, 20h
		mov rezbuf[di], 'X'
		mov rezbuf[di+1], 'O'
		mov rezbuf[di+2], 'R'
		call reg
		call modrm
		mov cx, 0
		call lseek
		mov cx,0
		mov cl, poslinkisisproceduros
		call naudojamaskodas
		call spausdinti
		jmp begin
         _0011_01:
		cmp al, 00000100b
		jne netrys
		mov di, 20h
		mov rezbuf[di], 'X'
		mov rezbuf[di+1], 'O'
		mov rezbuf[di+2], 'R'
		call akumuliatorius
		call bopr 
		call lseek
        
		mov cl, poslinkisisproceduros
       
		call naudojamaskodas
		call spausdinti

     jmp begin
		
        netrys:
     
    ret
endp trys
     
    
    proc du
        mov al, 11111000b
        and al, skbuf[0]
        cmp al, 00101000b
        jne nedu
        mov al, 00000100b
        and al, skbuf[0]
        _0010_10:
            cmp al, 0
            jne _0010_11 
            call dukodas
            call reg
            call modrm
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin
        
        _0010_11:
        call dukodas
        call akumuliatorius
        call bopr 
        call lseek
        
        mov cl, poslinkisisproceduros
       
        call naudojamaskodas
        call spausdinti

     jmp begin
        
        
       nedu: 
    ret  
endp du  
    
    proc dvylikakodas
        mov di, 20h
        mov cx, 0004h
        dvylikakodasloop:
        mov al, dvyliktukai[si]
        mov rezbuf[di], al
        mov al, dvyliktukai[si+1]
        mov rezbuf[di+1], al
        mov al, dvyliktukai[si+2]
        mov rezbuf[di+2], al
        mov al, dvyliktukai[si+3]
        mov rezbuf[di+3], al
        
        loop dvylikakodasloop
        add di, 4
    ret 
endp dvylikakodas  
    
    
    proc dukodas
         mov cx,0004h
        mov si,0
        mov di,20h
        dukodasloop:
        mov al, dvejetai[si]
        mov rezbuf[di], al
        inc si
        inc di 
        
        loop dukodasloop
    
    ret
endp dukodas
    
    proc tryskodas
         mov cx,0004h
        mov si,0
        mov di,20h
        tryskodasloop:
        mov al, trejetai[si]
        mov rezbuf[di], al
        inc si
        inc di 
        
        loop tryskodasloop
    
    ret
endp tryskodas
    
    
    
    proc penki
        mov al, 11110000b
        and al, skbuf[0]
        cmp al, 01010000b
        jne nepenki
        mov rezbuf[66], ' '
        mov al, 00001000b
        and al, skbuf[0]
        _0101_0:
        cmp al, 00000000b
        jne _0101_1
        mov si, 0
        jmp penkiveiksmai
        _0101_1:
        mov si, 4

        penkiveiksmai:
        call penkikodas
        mov ax, 0
        mov al, skbuf[0]
        and al, 00000111b
        mov cx, ax
        call xreg
        mov poslinkisisproceduros, 1
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin
 
        nepenki:
  
    ret
endp penki
       
    proc keturi
        mov al, 11110000b
        and al, skbuf[0]
        cmp al, 01000000b
        jne neketuri
        mov rezbuf[66], ' '
        mov al, 00001000b
        and al, skbuf[0]
        _0100_0:
        cmp al, 00000000b
        jne _0100_1
        mov si, 0
        jmp keturiveiksmai
        _0100_1:
        mov si, 4

        keturiveiksmai:
        call keturikodas
        mov ax, 0
        mov al, skbuf[0]
        and al, 00000111b
        mov cx, ax
        call xreg
        mov poslinkisisproceduros, 1
        mov cx, 0
        call lseek
        mov cl, poslinkisisproceduros
        call naudojamaskodas 
        call spausdinti
        jmp begin

        neketuri:
 
    ret
endp keturi
    proc keturikodas
        mov di, 20h
        mov cx, 0004h
        keturikodasloop:
        mov al, ketvertai[si]
        mov rezbuf[di], al
        mov al, ketvertai[si+1]
        mov rezbuf[di+1], al
        mov al, ketvertai[si+2]
        mov rezbuf[di+2], al
        mov al, ketvertai[si+3]
        mov rezbuf[di+3], al
        
        loop keturikodasloop

    ret 
endp keturikodas  
      
    proc penkikodas
        mov di, 20h
        mov cx, 0004h
        penkikodasloop:
        mov al, penketai[si]
        mov rezbuf[di], al
        mov al, penketai[si+1]
        mov rezbuf[di+1], al
        mov al, penketai[si+2]
        mov rezbuf[di+2], al
        mov al, penketai[si+3]
        mov rezbuf[di+3], al
        
        loop penkikodasloop

    ret
endp penkikodas

    neseptynijmp:
        jmp neseptyni
    
    proc septyni
    mov al, 11110000b
    and al, skbuf[0]
    cmp al, 01110000b
    jne neseptynijmp
    mov rezbuf[66], ' '
    mov al, 00001111b
    and al, skbuf[0]
    jump_0000:
    cmp al, 00000001b
    jge jump_0001 
    mov si, 0d
    jmp jumpveiksmai
    jump_0001:
    cmp al, 00000010b
    jge jump_0010 
    mov si, 4d
    jmp jumpveiksmai
    jump_0010:
    cmp al, 00000011b
    jge jump_0011 
    mov si, 8d
    jmp jumpveiksmai
    jump_0011:
    cmp al, 00000100b
    jge jump_0100 
    mov si, 12d
    jmp jumpveiksmai
    jump_0100:
    cmp al, 00000101b
    jge jump_0101 
    mov si, 16d
    jmp jumpveiksmai
    jump_0101:
    cmp al, 00000110b
    jge jump_0110 
    mov si, 20d
    jmp jumpveiksmai
    jump_0110:
    cmp al, 00000111b
    jge jump_0111 
    mov si, 24d
    jmp jumpveiksmai
    jump_0111:
    cmp al, 00001000b
    jge jump_1000 
    mov si, 28d
    jmp jumpveiksmai
    jump_1000:
    cmp al, 00001001b
    jge jump_1001 
    mov si, 32d
    jmp jumpveiksmai
    jump_1001:
    cmp al, 00001010b
    jge jump_1010 
    mov si, 36d
    jmp jumpveiksmai
    jump_1010:
    cmp al, 00001011b
    jge jump_1011 
    mov si, 40d
    jmp jumpveiksmai
    jump_1011:
    cmp al, 00001100b
    jge jump_1100 
    mov si, 44d
    jmp jumpveiksmai
    jump_1100:
    cmp al, 00001101b
    jge jump_1101 
    mov si, 48d
    jmp jumpveiksmai
    jump_1101:
    cmp al, 00001110b
    jge jump_1110 
    mov si, 52d
    jmp jumpveiksmai
    jump_1110:
    cmp al, 00001111b
    jge jump_1111 
    mov si, 56d
    jmp jumpveiksmai
    jump_1111: 
    mov si, 60d
    jmp jumpveiksmai 

    jumpveiksmai:
    call septynikodas
    call vienobaitoposlinkis
    mov poslinkisisproceduros, 2
    mov cx, 0
    call lseek
    mov cl, poslinkisisproceduros
    call naudojamaskodas 
    call spausdinti
    jmp begin
     
    neseptyni:
    ret
endp septyni
    proc septynikodas
        mov di, 20h
        mov cx, 0004h
        septynikodasloop:
        mov al, septynetai[si]
        mov rezbuf[di], al
        mov al, septynetai[si+1]
        mov rezbuf[di+1], al
        mov al, septynetai[si+2]
        mov rezbuf[di+2], al
        mov al, septynetai[si+3]
        mov rezbuf[di+3], al
        
        loop septynikodasloop

    ret
endp septynikodas

    proc vienobaitoposlinkis
        mov di, 25h 
        mov dx, poslinkisraw
        add dx, 2
        mov bx, 0
        mov bl, skbuf[1]
        ;mov al, 10000000b
        ;CMP bl, al
        and bl, 10000000b
        cmp bl, 0 
        je p0
        mov bh, 11111111b
        p0:
        mov bl, skbuf[1]
        add dx, bx
        push dx
        xchg dh, dl
        call makemetwochar
        mov rezbuf[di],dh
        mov rezbuf[di+1],dl
        pop dx
        call makemetwochar
        mov rezbuf[di+2],dh
        mov rezbuf[di+3],dl
    ret
endp vienobaitoposlinkis

    nepenkiolikajmp:
        jmp nepenkiolika
    
    proc penkiolika
        mov al, 11110000b
        and al, skbuf[0]
        cmp al, 11110000b
        jne nepenkiolikajmp
        mov rezbuf[66], ' '
        mov al, 00111000b
        and al, skbuf[1]
        _15_1_000:
            cmp al, 00000000b
            jne _15_1_001
            mov rezbuf[66], ' '
            mov si, 0d
            push si
            jmp ptrkodas
            
        _15_1_001:
            cmp al, 00001000b
            jne _15_0_100
            mov rezbuf[66], ' '
            mov si, 4d
            push si
            jmp ptrkodas
        _15_0_100:
            cmp al, 00100000b
            jne _15_0_110
            mov dl, 00001000b
            and dl, skbuf[0]
            cmp dl,0 ;000001000b
            je _15_0_110      
            mov si, 8d
            push si
            jmp ptrkodas
        _15_0_110:
            cmp al, 00110000b
            jne _15_1_010
            mov dl, 00001000b
            and dl, skbuf[0]
            cmp dl, 00001000b
            je _15_1_010
            mov si, 12d
            ;jmp ptrkodas
            jmp penkiolikaveiksmai
        _15_1_010:
            cmp al, 00010000b
            jne _15_1_011
            mov rezbuf[66], ' '
            mov si, 16d
            jmp penkiolikaveiksmai
        _15_1_011:
            cmp al, 00011000b
            jne _15_1_100
            mov rezbuf[66], ' '
            mov si, 20d
            jmp penkiolikaveiksmai
        _15_1_100:
            cmp al, 00100000b
            jne _15_1_101
            mov rezbuf[66], ' '
            mov si, 24d
            jmp penkiolikaveiksmai
        _15_1_101:
            cmp al, 00101000b
            jne _15_1_110
            mov rezbuf[66], ' '
            mov si, 28d
            jmp penkiolikaveiksmai
        _15_1_110:
            cmp al, 00110000b
            jne nepenkiolika
            mov rezbuf[66], ' '
            mov si, 32d
            jmp penkiolikaveiksmai  
        ptrkodas:
            mov al, skbuf[0]
            and al, 00000001h
            cmp al, 0
            jne wordptr
            mov di, 0d
            call ptrwkodas
            pop si
            jmp penkiolikaveiksmai
            
            wordptr:
            mov di, 8d
            call ptrwkodas
            pop si
        
        penkiolikaveiksmai:
            call _15code
            mov poslinkisisproceduros, 2
            call modrm
            
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin

        nepenkiolika:
    ret
endp penkiolika
     
    proc ptrwkodas
        push si
        mov cx, 0008h
        mov si, 25h
        ptrkodaloop:
        mov dl, ptrtxt[di]
        mov rezbuf[si], dl
        inc di
        inc si
        loop ptrkodaloop
        pop si
        
        
    ret 
endp ptrwkodas   
     
    
    proc _15code             ;si paduoti nuo kuo kur spausdinti
        mov cx, 0004h
        mov di, 20h
        _15codeloop:
        mov dl, penkiolikai[si]
        mov rezbuf[di], dl
        inc di
        inc si
        
        loop _15codeloop  
    ret 
endp _15code
      
     proc neatpazinta
        mov rezbuf[66], ' '
        mov poslinkisisproceduros, 1
        mov cx, 0001h
        call naudojamaskodas
        mov di, 20h
        mov si, 0h
        mov cx, 11d
        neatpazintasloop:
        mov dl, na[si]
        mov rezbuf[di],dl
        inc si
        inc di
        loop neatpazintasloop
        call lseek 
        call spausdinti
        
     ret
endp neatpazinta
     
     proc cleanrez
     mov si, 0
     mov cx, 140d
     cleanloop:
     mov rezbuf[si], ' ' 
     mov rezbuft[si], ' '
     inc si
     
     loop cleanloop   
        
     ret
endp cleanrez
 
     proc nulis
        mov al, 11111000b
        and al, skbuf[0]
        cmp al, 0
        jne nenulis
        mov al, 00000100b
        and al, skbuf[0]
        cmp al, 0                 
        
        jne add2
        add1:
            call addkodas
            call reg
            call modrm
            mov cx, 0
            call lseek
            mov cx,0
            mov cl, poslinkisisproceduros
            call naudojamaskodas
            call spausdinti
            jmp begin

        add2:
        call addkodas
        call akumuliatorius
        call bopr 
        call lseek
        
        mov cl, poslinkisisproceduros
       
        call naudojamaskodas
        call spausdinti

     jmp begin
        nenulis:
     ret
endp nulis
 
     proc akumuliatorius
        mov di, 25h
        mov rezbuf[di], 'a'
        mov al, 00000001b
        and al, skbuf[0]
        cmp al, 0
        je akumal
        mov rezbuf[di+1], 'x' 
        jmp akumend
        akumal:
        mov rezbuf[di+1], 'l' 
        akumend:
  
     ret
endp akumuliatorius
     
     proc bopr
        mov di, 45h
        mov al, 00000001b
        and al, skbuf[0]
        cmp al, 0
        je _1bopr
        mov poslinkisisproceduros, 3
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[di],dh
        mov rezbuf[di+1],dl
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[di+2],dh
        mov rezbuf[di+3],dl
        
        jmp boprend
        _1bopr:  
        mov poslinkisisproceduros, 2
        mov dl, skbuf[1]
        call makemetwochar
        mov rezbuf[di],dh
        mov rezbuf[di+1],dl
        
        boprend:
     ret
endp bopr
 
     proc xreg   ;paduoti cx kad gauti maza AX 
        mov di, 25h
        mov si, 0
        inc cx
        xregloop:
        mov al, w1[si]
        mov rezbuf[di],al
        mov al, w1[si+1]
        mov rezbuf[di+1],al
        add si, 2
        
        loop xregloop    
     ret 
endp xreg  
     
     
     proc reg               ;apskaiziuoja registra pagal reg ir w
        mov cx, 0000h
        mov si, 0
        mov al, 00000010b
        and al, skbuf[0]
        cmp al, 0
        je d0
        mov di, 30h
        jmp regtoliau
        d0: 
            mov di, 45h
            jmp regtoliau
             
        regtoliau:
        mov al, 00000001b
        and al, skbuf[0]
        cmp al, 0
       
        je wlygu0
        jmp wlygu1
        wlygu0:
        
        mov cl, 00111000b
        and cl, skbuf[1]
        ror cl, 3
        inc cl
        regloop1:
        mov bl, w0[si]
        mov rezbuf[di], bl
        mov bl, w0[si+1]
        mov rezbuf[di+1], bl
        add si, 2
        loop regloop1
        jmp regpabaiga
        
        wlygu1:
        
        mov cl, 00111000b
        and cl, skbuf[1]
        ror cl, 3
        inc cl
        regloop2:
        mov bl, w1[si]
        mov rezbuf[di], bl
        mov bl, w1[si+1]
        mov rezbuf[di+1], bl
        add si, 2
        loop regloop2
             
     regpabaiga:

     ret
endp reg

     proc addkodas
        mov cx,0004h
        mov si,0
        mov di,20h
        addkodasloop:
        mov al, nuliai[si]
        mov rezbuf[di], al
        inc si
        inc di 
        
        loop addkodasloop   
 
     ret
endp addkodas

     proc naudojamaskodas     ;paduoti cl kiek nori baitu
        
        push di  
        push si
        push ax
        mov di, 0
        cmp srn, 0
        je diplius0
        add di, 2
        diplius0:
            
        
        mov bx, 0010h
        mov si, 0
        
        naudojamaskodasloop:
        mov ax, 0
        mov al, skbuf[si]
        div bl
        mov dl, al
        call ascii
        mov rezbuf[di+8], dl
        mov dl, ah
        call ascii
        mov rezbuf[di+9], dl
        inc si
        add di, 2h

        loop naudojamaskodasloop
        pop ax
        pop si
        pop di
     ret
endp naudojamaskodas
     
     proc modrm
        
        mov cx, 0000h
        mov si, 0
        mov al, 00000010b
        and al, skbuf[0]
        cmp al, 0
        je modrmd0
        mov di, 49h
        jmp modrmtoliau1
        
        modrmd0: 
            mov di, 33h
            jmp modrmtoliau1 
        
        modrmtoliau1:
            mov al, 11000000b
            and al, skbuf[1]
            cmp al, 11000000b
            je mod11
            jmp modrmtoliau2
            
        mod11: 
        ;mov cx, 0002h
         ;call naudojamaskodas
        mov poslinkisisproceduros, 2h
        mov al, 00000001b
        and al, skbuf[0]
        cmp al, 0
        
        je modrmw0 
        jmp modrmw1
        
        modrmw0:
        push ax
	push bx
	mov al, 00000111b
        and al, skbuf[1]
        mov bl, 2
	mul bl
	mov ah, 0 
	mov si, ax
	;inc cl
        pop bx
	pop ax
        
	;modrmloop1:

	
	
	
	
	
	modrmloop1:
        mov bl, w0[si]
        mov rezbuf[di], bl
        mov bl, w0[si+1]
        mov rezbuf[di+1], bl
        ;add si, 2 
        ;loop modrmloop1
        jmp modrmpabaiga
        
        modrmw1:
        mov cl, 00000111b
        and cl, skbuf[1]
        inc cl
        
        modrmloop2:
        mov bl, w1[si]
        mov rezbuf[di], bl
        mov bl, w1[si+1]
        mov rezbuf[di+1], bl
        add si, 2 
        loop modrmloop2
        jmp modrmpabaiga
  
        mod1001:
        
        mov al, 11000000b
        and al, skbuf[1]
        cmp al, 01000000b
 
        je mod1001loop1
        jmp mod1001loop2
        
        mod1001loop1:
        mov poslinkisisproceduros, 3h
        mov cl, 00000111b
        and cl, skbuf[1]
        mov si, 1 
        inc cl
        cmp srn, 0
        je mod1001loop11
        mov rezbuf[di-1], ':'
        mov si, srnr
        mov dl, segregai[si]
        mov rezbuf[di-3], dl
        mov dl, segregai[si+1]
        mov rezbuf[di-2], dl
        mov si, 1
        mod1001loop11:
        mov rezbuf[di], '['
        mov al, mod12opr[si+1]
        mov rezbuf[di+1],al
        mov al, mod12opr[si+2]
        mov rezbuf[di+2],al
        mov al, mod12opr[si+3]
        mov rezbuf[di+3],al
        mov al, mod12opr[si+4]
        mov rezbuf[di+4],al
        mov al, mod12opr[si+5]
        mov rezbuf[di+5],al
        mov al, mod12opr[si+6]
        mov rezbuf[di+6],al
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[di+7],dh
        mov rezbuf[di+8],dl
        mov rezbuf[di+9], ']'
        add si, 10d

        loop mod1001loop11   
        
        jmp modrmpabaiga 
        
        mod1001loop2:
        mov poslinkisisproceduros, 4h
        mov cl, 00000111b
        and cl, skbuf[1]
        mov si, 1 
        inc cl
        cmp srn, 0
        je mod1001loop22
        mov rezbuf[di-1], ':'
        mov si, srnr
        mov dl, segregai[si]
        mov rezbuf[di-3], dl
        mov dl, segregai[si+1]
        mov rezbuf[di-2], dl
        mov si, 1
        
        
        
        mod1001loop22:
        
        mov rezbuf[di], '['
        mov al, mod12opr[si+1]
        mov rezbuf[di+1],al
        mov al, mod12opr[si+2]
        mov rezbuf[di+2],al
        mov al, mod12opr[si+3]
        mov rezbuf[di+3],al
        mov al, mod12opr[si+4]
        mov rezbuf[di+4],al
        mov al, mod12opr[si+5]
        mov rezbuf[di+5],al
        mov al, mod12opr[si+6]
        mov rezbuf[di+6],al
        mov dl, skbuf[3]
        call makemetwochar
        mov rezbuf[di+7],dh
        mov rezbuf[di+8],dl
        mov dl, skbuf[2]
        call makemetwochar
        mov rezbuf[di+9],dh
        mov rezbuf[di+10],dl
        mov rezbuf[di+11], ']'
        add si, 10d
        
        loop mod1001loop22  
        
		jmp modrmpabaiga 
       
        mod00:   
         ;mov cx, 0002h
         ;call naudojamaskodas
         cmp srn, 0
        je mod00nesr
        mov rezbuf[di-1], ':'
        mov si, srnr
        mov dl, segregai[si]
        mov rezbuf[di-3], dl
        mov dl, segregai[si+1]
        mov rezbuf[di-2], dl
       
        mod00nesr:
         mov poslinkisisproceduros, 02h
         mov dl, mod00opr[0]
         mov rezbuf[di], dl
         inc di
         
         mov cl, 00000111b
         and cl, skbuf[1]
         cmp cl, 00000110b
         je rm110 
         mov si, 2 
         jmp mod00tesiam
         rm110:
         mov poslinkisisproceduros, 04h
         ;mov cx, 0004h
         ;call naudojamaskodas
         mov dl, skbuf[2]
         call makemetwochar
        ; mov rezbuf[di], '['
         ;inc di
         mov rezbuf[di+2], dh
         inc di
         mov rezbuf[di+2], dl 
         inc di
         mov dl, skbuf[3]
         call makemetwochar
         
         mov rezbuf[di-2], dh
         inc di
         mov rezbuf[di-2], dl 
         inc di
         mov rezbuf[di], ']'
         jmp modrmpabaiga
         mod00tesiam:
          inc cx
 
        mod00ciklas:
        
        mov dl,mod00opr[si]
        mov rezbuf[di], dl
        mov dl,mod00opr[si+1]
        mov rezbuf[di+1], dl
        mov dl,mod00opr[si+2]
        mov rezbuf[di+2], dl
        mov dl,mod00opr[si+3]
        mov rezbuf[di+3], dl
        mov dl,mod00opr[si+4]
        mov rezbuf[di+4], dl
        mov dl, mod00opr[1]
        mov rezbuf[di+5], dl
        add si, 5 
        
        loop mod00ciklas
        jmp modrmpabaiga
        
        modrmtoliau2:          
            cmp al, 10000000b
            je mod1001jmp
            
            jmp modrmtoliau3
        modrmtoliau3:
            
            cmp al, 01000000b
            je mod1001jmp
            jmp modrmtoliau4
        modrmtoliau4:
            cmp al, 00000000b
            je mod00jmp
 
       modrmpabaiga:
     ret
endp modrm
     
     mod1001jmp:
        jmp mod1001
     
     mod00jmp:
        jmp mod00
     
     proc poslinkiskode;atspausdina ir paskaiciuoja nauja poslinki pridejus dx
     cmp srn,0
     mov rezbuf[0], '0'
     je poslsrne
     inc dx
     poslsrne:
     add poslinkisraw,dx
     mov cx, 0004h
     mov si, cx
     mov bl, 10h 
     add poslinkis[si-1], dl
     
     poslloop:   
        mov si, cx
        cmp poslinkis[si-1], 0Fh
        jg prideti1
        jmp spausdintiposlinki
     prideti1:
        inc poslinkis[si-2]    
        mov al, poslinkis[si-1]
        div bl
        mov poslinkis[si-1],ah
        loop poslloop
          
     spausdintiposlinki:  
     mov cx, 0004h
     posloop:
        mov si, cx
        mov dl,poslinkis[si-1]
        call ascii
        mov rezbuf[si-1], dl
     loop posloop    
     ret
endp poslinkiskode
    	   
	proc skaityti
	    mov bx, desk[0] 
	    mov ah, 42h
	    mov cx, 7h
	    mov dx, offset skbuf
	    mov ah, 3Fh
	    int 21h
	    jc err2
	    cmp ax, 0
	    je exitjmp1    
	ret
	endp skaityti
err2:
    jmp error2	
exitjmp1:
    jmp exit	
 
    proc parametrai
        mov ah, 0
        mov al, es:[0080h]
        cmp ax, 0
        jne yra_parametru
        jmp exit
    yra_parametru:
        mov si, 0
        mov bx, 0081h
        mov ax, es:[bx+1]
        mov bx, '/?'
        cmp ax, bx
        jne duomenu_skaitymas
        jmp helpm
     duomenu_skaitymas:
        mov bx, 0081h
        mov si, 0
     tarpas1:
        inc bx
        mov ax, es:[bx]
        cmp al, ' '
        je tarpas1
     duomfailio_vardas:
        mov duom[si], al
        inc si
        inc bx
        mov ax, es:[bx] 
        cmp al, ' '
        je rezfailio_tarpas
        jmp duomfailio_vardas
     rezfailio_tarpas:
        mov si, 0
        inc bx
        mov ax, es:[bx]
        cmp al, ' '
        je rezfailio_tarpas
        cmp al, 0dh
        jne rezfailis
        jmp exit  
     rezfailis:
        mov rez[si], al
        inc si
        inc bx
        mov ax, es:[bx]
        cmp al, 0dh
        jne rezfailis          
     ret
endp parametrai
    
    proc ascii
        cmp dl, 30h
        jge jauascii
        add dl, '0'
    jauascii:
        cmp dl, 39h
        jle skaicius
        add dl, 7h
    skaicius:
        cmp dl, 46h
        jle pabaiga
        sub dl, 7h        
    pabaiga:   
    ret
endp ascii  
    
    proc spausdinti             ;atspausdina rezultaztu buferi
        call sutvarkytitarpus
        
        mov bx, desk[2]
        mov cx, 0060h
        mov dx, offset rezbuft
        mov ah, 40h
        int 21h
        mov bx, desk[2]
        mov cx, 2h
        mov dx, offset enter
        mov ah, 40h
        int 21h           
    ret
endp spausdinti    
    
    proc sutvarkytitarpus
       mov si, 0
       mov di, 0 
       tloop:
       cmp si, 25h
       jne tvarkom
       jmp postok
       
       tvarkom:
       mov dl, rezbuf[si]
       mov rezbuft[di], dl
       inc si
       inc di
       jmp tloop
       
       postok: 
       
       cmp si, 45h
       jne darbiskis
       jmp almost
       darbiskis:
       cmp rezbuf[si], ' '
       je nsp1
       mov dl, rezbuf[si]
       mov rezbuft[di], dl
       inc si
       inc di
       jmp postok
       nsp1:
       inc si
       jmp postok
      
       almost:
       cmp si, 60h
       je sutvarkyta
       nufsio:
       cmp rezbuf[si], ' '
       je nunu
       mov dl, rezbuf[si]
       mov rezbuft[di], dl
       inc si
       inc di
       jmp almost 
       nunu:
       inc si
       jmp almost
       
      sutvarkyta:
    ret
endp sutvarkytitarpus
    
    proc lseek  ;paduoti dl koks reikalingas poslinkis duomenu faile
        
        mov bx, desk[0]
        mov cx, 0000h;failopozicija
        mov dx, failopozicija
        mov ax, 0
        mov al, poslinkisisproceduros
        add dx, ax
        mov ax, 0
        mov ah, 42h
        mov failopozicija, dx
        int 21h
    ret
endp lseek
        
    proc makemetwochar  ;paduodamas dl ir is jo padaromi du charai kuriuosjau galima delti i rezultata
    mov ax, 0
    mov al, dl
    mov bl, 10h
    div bl
    mov dl, al
    mov dh, ah
    call ascii
    xchg dh, dl
    call ascii     
    
    ret
endp makemetwochar
    
    proc open
        mov ax, 0
        mov ah, 3dh
        mov al, 0
        mov dx, offset duom
        int 21h
        jnc duomopen
        jmp error1
    duomopen:
        mov desk[0], ax
        mov ax, 0
        mov ah, 3ch
        mov cx, 0
        mov dx, offset rez
        int 21h
        jnc dsk
        jmp exit
        dsk:
        mov desk[2], ax 
     
     ret
endp open

    error1:
        mov ah, 9h
        mov dx, offset er1
        int 21h
        mov bx, desk[0]
        mov ah, 3Eh
        int 21h
        mov bx, desk[2]
        mov ah, 3Eh
        int 21h 
        mov ax, 4c00h
        int 21h
    error2:
        mov ah, 9h
        mov dx, offset er2
        int 21h
        mov bx, desk[0]
        mov ah, 3Eh
        int 21h
        mov bx, desk[2]
        mov ah, 3Eh
        int 21h 
        mov ax, 4c00h
        int 21h
        
    
    exit:
       
	    mov bx, desk[0]
        mov ah, 3Eh
        int 21h
        mov bx, desk[2]
        mov ah, 3Eh
        int 21h 
        mov ax, 4c00h
        int 21h
    helpm:
        mov ah, 9h
        mov dx, offset help
        int 21h
        mov bx, desk[0]
        mov ah, 3Eh
        int 21h
        mov bx, desk[2]
        mov ah, 3Eh
        int 21h 
        mov ax, 4c00h
        int 21h
end