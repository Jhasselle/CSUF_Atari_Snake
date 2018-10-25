; Jason Hasselle
; The final snake game
; Has: bound checking (walls), direction check (can't move into self)
; apples increase speed, lose screen, and a win screen.

define snakeLength $00
define currentDirection $01
define speed $02
define appleLo $03
define appleHi $04
define appleColor $05
define snakeHeadL $10 
define snakeHeadH $11  
define snakeBodyStart $12

define ASCII_w $77
define ASCII_a $61
define ASCII_s $73
define ASCII_d $64

main:
  JSR initialize ;Setting up initial values
  JSR gameLoop ;Looping the "game" until snake bites the dust in $0600 land.
  
  ;Lots of manual labor here..
initialize:
  ;manually place first apple
  ;because, why not?
  LDA #$05
  STA appleHi
  LDA #$0F
  STA appleLo
  LDA #$0A
  STA (appleLo,x)

  ;Defining apple color
  LDA #$0A
  STA appleColor

  ;manually make direction downward.
  LDA #ASCII_s
  STA $FF

  ;and keep a record of our direction
  STA currentDirection

  ;Snake length is in terms of nibbles! Each pixel coordinate must be plotted as bytes,
  ; so real length is snakeLength / 2
  LDA #$04
  STA snakeLength

  ;Starting position of head
  ;First column, third pixel down
  LDA #$Af
  STA snakeHeadL 
  LDA #$02
  STA snakeHeadH

  ;Starting position of body
  ;First column, second pixel down
  LDA #$8f
  STA snakeBodyStart
  LDA #$02
  INX
  STA snakeBodyStart, X
  
  ;Starting position of tail
  ;First column, first pixel
  LDX snakeLength
  LDA #$6f
  STA snakeHeadL, X
  LDA #$02
  INX
  STA snakeHeadL, X
  
  ;Make snake head green
  LDX #$00
  LDA #$05
  STA (snakeHeadL,X)
  
  ;Make body start green
  STA (snakeBodyStart, X)
  
  RTS

gameLoop: ;Repeat until TopBottomWallCheck detects the snakeHeadH has reached 06
  JSR readKeys 
  JSR checkAppleCollision ;sees if apple was eaten or vanished
  JSR updateSnake ;updates with new snake pixel locations 
  JSR checkSnakeCollision ;sees if snake moves into self
  JSR drawSnake
  JSR slowDown
  JMP gameLoop

readKeys:
  LDY $FF ;the direction we want to go, but not necessarily can go.
  RTS
  
updateSnake: ;updates with new snake pixel locations 
  JSR shiftValues 
  JSR changeHeadPosition
  RTS

;Shift pixel locations up the body
shiftValues:
  LDX snakeLength

  loop: ;Starting from the tail and working our way up to body start
    DEX 
    LDA snakeHeadL, X ;grabs the high nibble of the adjacent lower memory address byte
    INX
    INX
    STA snakeHeadL, X ;stores it in the current high nibble of the
      ;memory address being updated
    DEX
    DEX
    DEX
    LDA snakeHeadL, X ;new low nibble
    INX
    INX
    STA snakeHeadL, X ;updating low nibble with new nibble
    DEX
    DEX
    CPX #$00 ;once X is 0, the queue (minus the head) has completed
    BNE loop
  RTS

changeHeadPosition: 
  ;The Y register will always have latest user input in a gameLoop cycle
  ;This method considers user input, then possible actions
  ;if impossible, then force current direction

  LDX currentDirection ;keeping track of direction

  CPY #ASCII_w ;User wants to go up?
  BEQ moveUp  ;If so, lets go to the moveUp action and see if
              ;that is possible based on their current direction.
              
  CPY #ASCII_a
  BEQ moveLeft

  CPY #ASCII_s 
  BEQ moveDown

  CPY #ASCII_d
  BEQ moveRight

  moveUp: ;We want to move up, but first..
  CPX #ASCII_s ;..are we currently moving down?
  BEQ moveDown ;if so, too bad. You can't move into your own body, snek.

  LDA snakeHeadL
  SEC
  SBC #$20
  STA snakeHeadL
  BCC decHead
  CLC
  LDX #ASCII_w
  STX currentDirection
  JMP finish

  moveLeft: ;Same as moveUp but with appropriate direction checks
  CPX #ASCII_d
  BEQ moveRight

  JSR leftWallCheck
  LDA snakeHeadL
  SEC
  SBC #$01
  CLC
  STA snakeHeadL
  LDX #ASCII_a
  STX currentDirection
  JMP finish

  moveDown:
  CPX #ASCII_w
  BEQ moveUp

  LDA snakeHeadL
  CLC
  ADC #$20
  STA snakeHeadL
  BCS incHead
  LDX #ASCII_s
  STX currentDirection
  JMP finish

  moveRight:
  CPX #ASCII_a
  BEQ moveLeft

  JSR rightWallCheck
  LDA snakeHeadL
  CLC
  ADC #$01
  STA snakeHeadL
  LDX #ASCII_d
  STX currentDirection

  finish: ;done with setting new head location
  RTS

incHead: ;Increment the byte if carry flag is up
  LDX snakeHeadH
  INX 
  STX snakeHeadH
  JSR TopBottomWallCheck ;Are we past the hi byte 6? If so, exit.
  RTS

decHead:
  LDX snakeHeadH
  DEX 
  STX snakeHeadH
  JSR TopBottomWallCheck
  RTS

TopBottomWallCheck:
  LDX snakeHeadH ;Are we past the hi byte 6 and entering $0600 land? If so, exit.
  CPX #$06 
  BEQ youLose
  LDX snakeHeadH 
  CPX #$01
  BEQ youLose
  RTS

drawSnake: ;Author: David McLaren (You! ..I think)
  LDA #$05
  LDX #$00
  STA (snakeHeadL, X) ;Indirect, x; Makes the head green
  LDA #$00
  LDX snakeLength
  STA (snakeHeadL, X) ;Make the tail black; we don't want an ever-growing snake
  LDX #$00
  RTS

;Author: David McLaren (You!)
rightWallCheck:
  LDA snakeHeadL
  AND #$1F
  CMP #$1F
  BEQ youLose ;Well, you shouldn't have died.
  RTS

;Author: David McLaren (You!)
leftWallCheck:
  LDA snakeHeadL
  AND #$1F
  CMP #$00
  BEQ youLose 
  RTS

;Check to see if the apple has been collided into by the snake.
checkAppleCollision:
  ;is the pixel where head is red?
  LDX #$00
  LDA (appleLo,X)
  CMP #$0A
  BNE growSnake
  LDA #$00
  RTS

;Check to see if the snake is colliding into itself.
checkSnakeCollision:
  LDX #$00
  LDA (snakeHeadL,X)
  CMP #$05 ;checking if new head position is already green, if so then...
  BEQ youLose ;<---
  LDA #$00 ;reset A; good manners?
  RTS

;Snek eat, snek grow.
growSnake:
  LDX snakeLength 
  INX ;increment by 2 to represent a byte increment
  INX 
  STX snakeLength
  JSR speedUp ;speed up the snake every time it eats
  JSR drawApple ;snake ate an apple (or spawned in body), then draw a new apple.
  RTS 

;Spawns red apple in quasi-random location
;Author: David McLarden (I think?)
;If not, then Jake Dappen helped me out
drawApple:
  LDA $FE
  AND #$03
  CLC
  ADC #$02
  STA appleHi
  LDA $FE
  STA appleLo
  LDX #$00
  LDA appleColor
  STA (appleLo,X)
  LDA #$00
  RTS

;Author: David McLaren (You!)
;I added"speed". Everything speeds up slightly
;upon eating an apple.
slowDown:
  LDX speed
  slowLoop:
    NOP
    INX
    CPX #$FF
    BNE slowLoop
  RTS

;Because we wanna go fast, don't we?
speedUp: 
  LDA speed
  ADC #$10 ;vroom vroom
  BCS youWin ;did you overflow? Congrats on breaking the speed of atari light!
  STA speed
  RTS

;Red Static (so spooky!)
youLose:
LDX #$00
STX $00
LDX #$02 ;Loads starting value 02 into X.
         ;This will be the 02 in 0200
loop1:
 STX $01 ;start of grid
 LDY #$00 ;Resets Y, which will traverse each row

 loop2:
  LDA $FE ;The magical rand generating memory address
  AND #$02
  STA ($00),Y ;Indirect Indexed
  CPY #$FF ;Did we loop 256 times? (first "quadrant")
  INY
  BNE loop2

INX ;Increment the hibyte
CPX #$06 ;Did we fill the screen?
BNE loop1
JMP end

;You must eat 16 apples to know what happens!
youWin:
  LDX #$00
  STX $00
  LDX #$02 
           
  loop3:
   STX $01 
   LDY #$00 

   loop4:
    LDA #$07 
    STA ($00),Y 
    CPY #$FF 
    INY
    BNE loop4

  INX 
  CPX #$06 
  BNE loop3

  LDA $00
  STA $03AC
  STA $03B3
  STA $0469
  STA $0476
  STA $048A
  STA $0495
  STA $04AB
  STA $04AC
  STA $04B3
  STA $04B4
  STA $04CD
  STA $04CE
  STA $04CF
  STA $04D0
  STA $04D1
  STA $04D2

  JMP end

end: 
  BRK ;RIP In Peace, snek.
