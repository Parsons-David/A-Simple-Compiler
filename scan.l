%{
#include <string.h>
#include "attr.h"
#include "parse.tab.h"
#define YY_SKIP_YYWRAP
%}
	extern YYSTYPE yylval;
        extern int yywrap() {return 1;};
	extern int isatty();
	int lines = 1;

D       [0-9]
id	[A-Za-z][A-Za-z0-9]*
qstring	\'[^\'\n]*[\'\n]

%%
{D}+		{ECHO;
		yylval.token.num = atoi(yytext);
		return(ICONST);}
:=		{ECHO;return(ASG);}
\.		{ECHO;return(PERIOD);}
"/*"		{
		int c1 = 0; int c2 = input();
		ECHO;
		for(;;) {
			if (c2 == EOF) {
				printf("ERROR: EOF detected in comment\n");
				yyterminate();
				}
			fprintf(yyout,"%c",c2);
			if (c1 == '*' && c2== '/') break;
			c1 = c2;
			c2 = input();
			if (c1 == '\n' && c2 != EOF)
				{lines++; fprintf(yyout,"%d\t",lines);}
			}
		}
[\[\]*+(),:;=-]	{ECHO;return(yytext[0]);}
program		{ECHO;return(PROG);}
var		{ECHO;return(VAR);}
integer		{ECHO;return(INT);}
begin		{ECHO;return(BEG);}
end		{ECHO;return(END);}
print		{ECHO;return(PRINT);}
{id}		{ECHO;
		yylval.token.str = (char *) malloc(strlen(yytext)+1);
		strcpy(yylval.token.str,yytext);
		return(ID);
		}
\n		{int c;
		ECHO;
		c = input();
		if (c != EOF) {unput(c); lines++; fprintf(yyout,"%d\t",lines);}
		else yyterminate();
		}
[ \t]		{ECHO;}
[!@#$%^&|?]	{ECHO;
		printf("\nERROR: %c is an illegal character\n",yytext[0]);
		}
%%
