       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDPND.
       AUTHOR. FABIANO SANTINI MARQUES.
      ***********************************************
      * MANUTENCAO DO CADASTRO DEPENDENTES   *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADDPND ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CHAPA
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADDPND
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADDPND.DAT".
       01 REGDPND.
                03 CHAPA               PIC 9(06).
                03 SEQ                 PIC 9(01).
                03 NOMEDPND            PIC X(30).
                03 GRAU                PIC 9(01).
				03 DATANASC            PIC 9(08).
				03 SEXO                PIC X(01).
				03 DPNDINSS            PIC X(01).
				03 DPNDIR              PIC X(01).
				03 STATS               PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL          PIC 9(01) VALUE ZEROS.
       77 W-CONT         PIC 9(06) VALUE ZEROS.
       77 W-OPCAO        PIC X(01) VALUE SPACES.
       77 ST-ERRO        PIC X(02) VALUE "00". 
       77 W-ACT          PIC 9(02) VALUE ZEROS.
       01 TXGRAU         PIC X(14) VALUE SPACES.
       01 TXSTATS        PIC X(16) VALUE SPACES.
       01 TXSEXO         PIC X(12) VALUE SPACES.
       01 TXINSS         PIC X(12) VALUE SPACES.	
       01 TXIR           PIC X(12) VALUE SPACES.	   
       77 MENS           PIC X(50) VALUE SPACES.
       77 LIMPA          PIC X(50) VALUE SPACES.
      *
       01 TABGRAU.
          03 FILLER        PIC X(14) VALUE " ESPOSA".
          03 FILLER        PIC X(14) VALUE " FILHO(A)".
          03 FILLER        PIC X(14) VALUE " ENTEADO(A)".
          03 FILLER        PIC X(14) VALUE " PAI".
          03 FILLER        PIC X(14) VALUE " MAE".
          03 FILLER        PIC X(14) VALUE " AVOS".
          03 FILLER        PIC X(14) VALUE " NETO(A)".
          03 FILLER        PIC X(14) VALUE " AGREGADO".
          03 FILLER        PIC X(14) VALUE " RESP. LEGAL".	
          03 FILLER        PIC X(14) VALUE " OUTROS".
      *			  
       01 TABAUX REDEFINES TABGRAU.
           03 TTIPGRAU        PIC X(14) OCCURS 10 TIMES. 		  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  CDEPEND REQUIRED BACKGROUND-COLOR 7.
	       05 BACKGROUND-COLOR 7.
           05  BLANK SCREEN.
           05  LINE 24  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 24  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 25  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  6.			   
           05  LINE 25  COLUMN 41 
               VALUE  "             * SANTINI SOLUTIONS (C) *  "
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.
           05  LINE 01  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR  1.
           05  LINE 01  COLUMN 41 
               VALUE  "                                        "
			   BACKGROUND-COLOR  1.		   
           05  LINE 02  COLUMN 01 
               VALUE  "                   C A D A S T R O  D E "
			   FOREGROUND-COLOR  15.
           05  LINE 02  COLUMN 41 
               VALUE  " D E P E N D E N T E S                  ".
           05  LINE 03  COLUMN 01 
               VALUE  " _______________________________________"
			   FOREGROUND-COLOR  0.			   
           05  LINE 03  COLUMN 41 
               VALUE  "_______________________________________ "
			   FOREGROUND-COLOR  0.			   
           05  LINE 04  COLUMN 01 
               VALUE  " _______________________________________".
           05  LINE 04  COLUMN 41 
               VALUE  "_______________________________________ ".		   
           05  LINE 05  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR  7.	
           05  LINE 05  COLUMN 41 
               VALUE  "                                        "
			   BACKGROUND-COLOR  7.
           05  LINE 06  COLUMN 01 
               VALUE  "                               "
			   BACKGROUND-COLOR  7.			   
           05  LINE 06  COLUMN 32 
               VALUE  "CHAPA : "
			   FOREGROUND-COLOR  6.
           05  LINE 07  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 07  COLUMN 41 
               VALUE  "_______________________________________ "
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.				   
           05  LINE 09  COLUMN 01 
               VALUE  "  SEQUENCIA DEPENDENTE :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 10  COLUMN 01 
               VALUE  "  NOME DO DEPENDENTE :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 11  COLUMN 01 
               VALUE  "  GRAU DE PARENTESCO :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 12  COLUMN 01 
               VALUE  "  DATA DE NASCIMENTO :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 13  COLUMN 01 
               VALUE  "  SEXO :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 14  COLUMN 01 
               VALUE  "  DEPENDENTE INSS :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 15  COLUMN 01 
               VALUE  "  DEPENDENTE IMP. RENDA :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 16  COLUMN 01 
               VALUE  "  STATUS :"
			   FOREGROUND-COLOR  0.			   
           05  LINE 18  COLUMN 01 
               VALUE  " _______________________________________".
           05  LINE 18  COLUMN 41 
               VALUE  "_______________________________________".			   
           05  TCHAPA
               LINE 06  COLUMN 40  PIC 9(06)
               USING  CHAPA
			   FOREGROUND-COLOR  6.
           05  TSEQ
               LINE 09  COLUMN 26  PIC 9(01)
               USING  SEQ
			   FOREGROUND-COLOR  0.			   
           05  TNOMEDPND
               LINE 10  COLUMN 24  PIC X(30)
               USING  NOMEDPND.
           05  TGRAU
               LINE 11  COLUMN 24  PIC 9(01)
               USING  GRAU.
           05  TTXGRAU
               LINE 11  COLUMN 26  PIC X(14)
               USING  TXGRAU.				   
           05  TDATANASC
               LINE 12  COLUMN 24  PIC 99/99/9999
               USING  DATANASC.
           05  TSEXO
               LINE 13  COLUMN 10  PIC X(01)
               USING  SEXO.
           05  TTXSEXO
               LINE 13  COLUMN 11  PIC X(12)
               USING  TXSEXO.			   
           05  TDPNDINSS
               LINE 14  COLUMN 21  PIC X(01)
               USING  DPNDINSS.
           05  TTXINSS
               LINE 14  COLUMN 23  PIC X(12)
               USING  TXINSS.			   
           05  TDPNDIR
               LINE 15  COLUMN 27  PIC X(01)
               USING  DPNDIR.
           05  TTXIR
               LINE 15  COLUMN 29  PIC X(12)
               USING  TXIR.			   
           05  TSTATS
               LINE 16  COLUMN 12  PIC X(01)
               USING  STATS.
           05  TTXSTATS
               LINE 16  COLUMN 14  PIC X(16)
               USING  TXSTATS.
      *	   
       01  TBGR.	   
	       05  LINE 08  COLUMN 52
               VALUE  "| ** GRAU DE PARENTESCO **|"
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.		   
           05  LINE 09  COLUMN 52 
               VALUE  "|    0: ESPOSA            |".
           05  LINE 10  COLUMN 52 
               VALUE  "|    1: FILHO(A)          |".
           05  LINE 11  COLUMN 52 
               VALUE  "|    2: ENTEADO(A)        |".
           05  LINE 12  COLUMN 52 
               VALUE  "|    3: PAI               |".
           05  LINE 13  COLUMN 52 
               VALUE  "|    4: MAE               |".
           05  LINE 14  COLUMN 52 
               VALUE  "|    5: AVOS              |".
           05  LINE 15  COLUMN 52  
               VALUE  "|    6: NETO(A)           |".
           05  LINE 16  COLUMN 52 
               VALUE  "|    7: AGREGADO          |".
           05  LINE 17  COLUMN 52 
               VALUE  "|    8: RESPONSAVEL LEGAL |".	
           05  LINE 18  COLUMN 52 
               VALUE  "|    9: OUTROS            |".			   
	       05  LINE 19  COLUMN 52
               VALUE  "|_________________________|".
      *	   
       01  DPNDSTATS.	   
	       05  LINE 12  COLUMN 52
               VALUE  "| ****** STATUS ********* |"
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.			   
           05  LINE 13  COLUMN 52 
               VALUE  "|    A - ATIVA            |".
           05  LINE 14  COLUMN 52 
               VALUE  "|    S - SUSPENSA         |".
           05  LINE 15  COLUMN 52 
               VALUE  "|    D - DESATIVADA       |".
           05  LINE 16  COLUMN 52  
               VALUE  "|    X - EX DEPENDENTE    |".			   
	       05  LINE 17  COLUMN 52
               VALUE  "|_________________________|".				   
      *			   
       01  SCRINSS.	   
	       05  LINE 12  COLUMN 52
               VALUE  "| ********* INSS ******** |"
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.   
           05  LINE 13  COLUMN 52 
               VALUE  "|     S - SIM             |".
           05  LINE 14  COLUMN 52 
               VALUE  "|     N - NAO             |".
           05  LINE 15  COLUMN 52 
               VALUE  "|     E - ESPECIAL        |".			   
	       05  LINE 16  COLUMN 52
               VALUE  "|_________________________|".	  
      *			   
       01  SCRIR.	   
	       05  LINE 12  COLUMN 52
               VALUE  "| ******** IRRF ********* |"
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.		   
           05  LINE 13  COLUMN 52 
               VALUE  "|     S - SIM             |".
           05  LINE 14  COLUMN 52 
               VALUE  "|     N - NAO             |".
           05  LINE 15  COLUMN 52 
               VALUE  "|     E - ESPECIAL        |".			   
	       05  LINE 16  COLUMN 52
               VALUE  "|_________________________|".	 			   
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       
       R0.
           OPEN I-O CADDPND
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADDPND
                 CLOSE CADDPND
                 MOVE "*** ARQUIVO CADDPND FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADDPND" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO NOMEDPND SEXO DPNDINSS TXSTATS 
		   MOVE SPACES TO DPNDIR STATS TXGRAU TXSEXO
		   MOVE SPACES TO TXINSS TXIR
           MOVE ZEROS  TO CHAPA SEQ GRAU DATANASC.
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY CDEPEND.
      *-------------[ ENTRADA DA CHAPA ]--------------------------------
       R2.
           DISPLAY (20, 29) "TECLAR ESC PARA SAIR "
           ACCEPT TCHAPA
           DISPLAY (20, 29) "                      "
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   CLOSE CADDPND
                   GO TO ROT-FIM.
           IF W-ACT = 02
                     MOVE "*** TECLEI O F1 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF W-ACT = 10
                     MOVE "*** TECLEI O F9 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF CHAPA = ZEROS
                     MOVE "*** DADO NAO PODE SER ZEROS **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.   
       LER-CADMDPND.
           MOVE 0 TO W-SEL
           READ CADDPND
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00" OR "02"
                DISPLAY CDEPEND
                MOVE "*** CHAPA JA CADASTRADA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADDPND" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CHAPA NAO CADASTRADA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TSEQ.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF SEQ = ZEROS
                     MOVE "*** DADO NAO PODE SER ZEROS **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R3. 		  	   
       R4.
           ACCEPT TNOMEDPND.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF NOMEDPND = SPACES
                     MOVE "*** NAO PODE FICAR EM VAZIO**" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R4.				   
       R5.
           DISPLAY TBGR.
           ACCEPT TGRAU.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
         R5A.
           IF GRAU = "0"  MOVE "ESPOSA" TO TXGRAU
           ELSE
		    IF GRAU = "1"  MOVE "FILHO(A)" TO TXGRAU
            ELSE		   
		     IF GRAU = "2" MOVE "ENTEADO(A)" TO TXGRAU
			 ELSE
		      IF GRAU = "3" MOVE "PAI" TO TXGRAU
              ELSE		   
		       IF GRAU = "4" MOVE "MAE" TO TXGRAU
			   ELSE
		        IF GRAU = "5" MOVE "AVOS" TO TXGRAU
                ELSE		   
		         IF GRAU = "6" MOVE "NETO(A)" TO TXGRAU	
                 ELSE		   
		          IF GRAU = "7" MOVE "AGREGADO" TO TXGRAU
			      ELSE
		           IF GRAU = "8" MOVE "RESP. LEGAL" TO TXGRAU
                   ELSE		   
		            IF GRAU = "9" MOVE "OUTROS" TO TXGRAU				 
                    GO TO R5.					
       R6.
		   DISPLAY CDEPEND.	
           ACCEPT TDATANASC.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R7.
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
         R7A.
           IF SEXO = "M" OR "m" MOVE " MASCULINO" TO TXSEXO
           ELSE
            IF SEXO = "F" OR "f" MOVE " FEMENINO " TO TXSEXO 
            ELSE
             GO TO R7.
             DISPLAY TTXSEXO.				   
       R8.
	       DISPLAY SCRINSS.	 
           ACCEPT TDPNDINSS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
         R8A.
           IF DPNDINSS = "S"  MOVE " SIM" TO TXINSS
           ELSE
            IF DPNDINSS = "N" MOVE " NAO " TO TXINSS 
            ELSE
			 IF DPNDINSS = "E" MOVE " ESPECIAL " TO TXINSS 
             ELSE
			  GO TO R8.
              DISPLAY TTXINSS.					   
       R9.
	       DISPLAY SCRIR.
           ACCEPT TDPNDIR.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
         R9A.
           IF DPNDIR = "S"  MOVE " SIM" TO TXIR
           ELSE
            IF DPNDIR = "N" MOVE " NAO " TO TXIR 
            ELSE
			 IF DPNDIR = "E" MOVE " ESPECIAL " TO TXIR
             ELSE
			  GO TO R9.
              DISPLAY TTXIR.					   
       R10.
	       DISPLAY DPNDSTATS.
           ACCEPT TSTATS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9.
         R10A.
           IF STATS = "A" MOVE "ATIVA" TO TXSTATS
           ELSE
		    IF STATS = "S" MOVE "SUSPENSA" TO TXSTATS 
            ELSE		   
		     IF STATS = "D" MOVE "DESATIVADA" TO TXSTATS
		     ELSE
			  IF STATS = "X" MOVE "EX DEPENDENTE" TO TXSTATS
			  ELSE
               GO TO R10.
			   DISPLAY TTXSTATS.
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (20, 29) "GRAVAR (S/N) : ".
                ACCEPT (20, 46) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGDPND
                IF ST-ERRO = "00"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU
					  ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* CHAPA JA EXISTE,DADOS NAO GRAVADOS *"
				  TO MENS
                  PERFORM ROT-MENS
				  THRU ROT-MENS-FIM
                  GO TO ACE-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE DEPENDENTE" 
					  TO MENS
                      PERFORM ROT-MENS
					  THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
	            DISPLAY (20, 02) " N = NOVO REGISTRO".
				DISPLAY (21, 02) " A = ALTERAR REGISTRO".
				DISPLAY (22, 02) " E = EXCLUIR REGISTRO".
                DISPLAY (23, 02) " DIGITE SUA OPCAO: ".
                ACCEPT (23, 21) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (22, 35) "      VOCE DESEJA EXCLUIR (S/N)?:".
                ACCEPT (22, 71) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADDPND RECORD
                IF ST-ERRO = "00"
                   MOVE "** REGISTRO DEPENDENTE EXCLUIDO **" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (20, 29) "ALTERAR  (S/N) : ".
                ACCEPT (20, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGDPND
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO FORNECEDOR"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADDPND.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (20, 26) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (20, 26) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.


      

