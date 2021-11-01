%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct node {
	struct node *left;
	struct node *right;
	char *token;
} node;

typedef struct symbolTableStruct {
	char *type;
	char *id;
	char *value;
	int isFunc;
	int level;
	node *funcAddress;
} symbolTableStruct;

#define SIZE_ADD 10

int currentLevel = 0;
int lastSymbol = 0;
//int sizeAdd = 10;
int symbolTableSize = SIZE_ADD;
symbolTableStruct *symbolTable;
int errors = 0;
int mainMethod = 0;
node *root;

node *mknode(node *left, node *right, char *token);
void printTree(node *tree, int tab);
void addSymbol(node *tree, int lvl);
void checkTree(node *tree, int lvl);
void startCheckTree(node *tree, int lvl);
int symbolNumber(char *id, int lvl, int isFunc);
int checkFunc(node *tree);
node *findFuncDeclar(node *tree, char *id);
void cleanTableLvl(int lvl);

void errorPrint(int errorCode, char *msg);
void yyerror(char *str);
int yylex(void);

extern char *yytext;
#define YYSTYPE struct node*

#include "lex.yy.c"
void yyerror(char *str) {
	printf("%s: %s (%d)\n", str, yytext, count);
}
%}

%start Start
%token AUTO STRUCT BREAK ELSE SWITCH CASE ENUM REGISTER TYPEDEF EXTERN RETURN UNION CONST UNSIGNED CONTINUE FOR SIGNED VOID DEFAULT GOTO SIZEOF VOLATILE DO IF STATIC WHILE VAL_NULL DOT B_NOT B_OR B_L_SHIFT B_R_SHIFT QUESTION ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN B_AND_ASSIGN B_OR_ASSIGN B_XOR_ASSIGN B_L_SHIFT_ASSIGN B_R_SHIFT_ASSIGN
%token IDENTIFIER
%token FLOAT_CONSTANT INT_CONSTANT CHAR_CONSTANT
%token BOOL DOUBLE INT LONG CHAR FLOAT SHORT P_DOUBLE P_INT P_LONG P_CHAR P_FLOAT P_SHORT 
%token DIR_INCLUDE LIBRARY DIR_DEFINE DIR_UNDEF DIR_IF DIR_IFDEF DIR_IFNDEF DIR_ELIF DIR_ELSE DIR_ENDIF DIR_LINE DIR_ERROR DIR_PRAGMA DIR_NULL
%token SEMICOLON COLON COMMA
%token O_PARENTH C_PARENTH O_BRACE C_BRACE O_BRACKET C_BRACKET
%token BOOL_TRUE BOOL_FALSE GREATER LESS EQUAL NOT_EQUAL GREATER_EQUAL LESS_EQUAL NOT AND OR
%token ASSIGN ADD SUB MUL DIV MOD INC DEC
%token POINTER P_ADDRESS

%left ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN B_OR_ASSIGN B_AND_ASSIGN B_XOR_ASSIGN B_L_SHIFT_ASSIGN B_R_SHIFT_ASSIGN
%left OR
%left AND
%left B_OR
%left B_XOR
%left B_AND
%left EQUAL NOT_EQUAL
%left GREATER LESS GREATER_EQUAL LESS_EQUAL
%left B_L_SHIFT B_R_SHIFT
%left ADD SUB
%left NOT B_NOT MUL DIV MOD
%left INC DEC

%%
Start: commands {printTree($1, 0); startCheckTree($1, 0);};

commands: commands command {$$ = mknode($1, $2, "");};
	| command {$$ = mknode($1, NULL, "");}
;

command: newVar | idExpr SEMICOLON | condition | loop | func /*| dir*/;

newVar: type id SEMICOLON {$$ = mknode($1, $2, "NEW");}
	| type idExpr SEMICOLON {$$ = mknode($1, $2, "NEW");}
;

type:	INT {$$ = mknode(NULL, NULL, "INT");}
	| FLOAT {$$ = mknode(NULL, NULL, "FLOAT");}
	| CHAR {$$ = mknode(NULL, NULL, "CHAR");}
	| BOOL {$$ = mknode(NULL, NULL, "BOOL");}
	| DOUBLE {$$ = mknode(NULL, NULL, "DOUBLE");}
	| LONG {$$ = mknode(NULL, NULL, "LONG");}
	| SHORT {$$ = mknode(NULL, NULL, "SHORT");}
	| P_INT {$$ = mknode(NULL, NULL, "INT");}
	| P_FLOAT {$$ = mknode(NULL, NULL, "FLOAT");}
	| P_CHAR {$$ = mknode(NULL, NULL, "CHAR");}
	| P_DOUBLE {$$ = mknode(NULL, NULL, "DOUBLE");}
	| P_LONG {$$ = mknode(NULL, NULL, "LONG");}
	| P_SHORT {$$ = mknode(NULL, NULL, "SHORT");}
;

typeVoid:	VOID {$$ = mknode(NULL, NULL, "VOID");};

id: 	IDENTIFIER {$$ = mknode(NULL, NULL, yytext);}
	| POINTER id {$$ = mknode(NULL, $2, "POINTER");}
	| P_ADDRESS id {$$ = mknode(NULL, $2, "ADDRESS");}
;

idExpr: id ASSIGN expr {$$ = mknode($1, $3, "=");}
	//| POINTER id ASSIGN expr {$$ = mknode($2, $4, "=");}
	//| P_ADDRESS id ASSIGN expr {$$ = mknode($2, $4, "=");}
;

expr: 	expr ADD expr {$$ = mknode($1, $3, "+");}
	| expr SUB expr {$$ = mknode($1, $3, "-");}
	| expr MUL expr {$$ = mknode($1, $3, "*");}
	| expr DIV expr {$$ = mknode($1, $3, "/");}
	| expr MOD expr {$$ = mknode($1, $3, "%");}
	| O_PARENTH expr C_PARENTH {$$ = mknode($2, NULL, "");}
	//| POINTER id {$$ = mknode(NULL, $2, "^");}
	//| P_ADDRESS id {$$ = mknode(NULL, $2, "&");}
	| id {$$ = mknode(NULL, $1, "ID");}
	| val {$$ = mknode(NULL, $1, "CONSTANT");}
;

val: 	int_const {$$ = mknode(NULL, $1, "INT");}
	| float_const {$$ = mknode(NULL, $1, "FLOAT");}
	| char_const {$$ = mknode(NULL, $1, "CHAR");}
;

int_const: INT_CONSTANT {$$ = mknode(NULL, NULL, yytext);};

float_const: FLOAT_CONSTANT {$$ = mknode(NULL, NULL, yytext);};

char_const: CHAR_CONSTANT {$$ = mknode(NULL, NULL, yytext);};

condition: IF condHead condBody {$$ = mknode($2, $3, "IF");};

condHead: O_PARENTH boolExpr C_PARENTH {$$ = mknode(NULL, $2, "COND");};

boolExpr: 	id {$$ = mknode(NULL, $1, "ID");}
		| val {$$ = mknode(NULL, $1, "CONSTANT");}
		| BOOL_TRUE {$$ = mknode(NULL, NULL, "TRUE");}
		| BOOL_FALSE {$$ = mknode(NULL, NULL, "FALSE");}
		| boolExpr GREATER boolExpr {$$ = mknode($1, $3, ">");}
		| boolExpr LESS boolExpr {$$ = mknode($1, $3, "<");}
		| boolExpr EQUAL boolExpr {$$ = mknode($1, $3, "==");}
		| boolExpr NOT_EQUAL boolExpr {$$ = mknode($1, $3, "!=");}
		| boolExpr GREATER_EQUAL boolExpr {$$ = mknode($1, $3, ">=");}
		| boolExpr LESS_EQUAL boolExpr {$$ = mknode($1, $3, "<=");}
		| boolExpr AND boolExpr {$$ = mknode($1, $3, "&&");}
		| boolExpr OR boolExpr {$$ = mknode($1, $3, "||");}
		| O_PARENTH boolExpr C_PARENTH {$$ = mknode($2, NULL, "");}
		| NOT boolExpr {$$ = mknode($2, NULL, "!");}
;

condBody: 	body {$$ = mknode($1, NULL, "");}
		| body ELSE body {$$ = mknode($1, $3, "");}
;

body: 	command {$$ = mknode($1, NULL, "");}
	| O_BRACE commands C_BRACE {$$ = mknode(NULL, $2, "BLOCK");}
;

loop: 	WHILE condHead body {$$ = mknode($2, $3, "WHILE");}
	| FOR forHead body {$$ = mknode($2, $3, "FOR");}
;

forHead: O_PARENTH forCounter boolExpr SEMICOLON idExpr C_PARENTH {$$ = mknode(mknode($2, $3, ""), $5, "COND");};

forCounter: 	idExpr SEMICOLON
		| type idExpr SEMICOLON {$$ = mknode($1, $2, "NEW");}
;

func: funcCall | funcDeclare;

funcCall: id O_PARENTH vars C_PARENTH SEMICOLON {$$ = mknode($1, $3, "METHOD");};

vars: 	{$$ = mknode(NULL, NULL, "");}
	| id {$$ = mknode(NULL, $1, "ID");}
	| id COMMA vars {$$ = mknode($1, $3, "ID");}
;

funcDeclare: 	type typeFunc {$$ = mknode($1, $2, "NEW");}
		| typeVoid voidFunc {$$ = mknode($1, $2, "NEW");}
;



typeFunc: funcNameVars typeFuncBody {$$ = mknode($1, $2, "FUNC");};

voidFunc: 	funcNameVars nameVoidBody {$$ = mknode($1, $2, "FUNC");};
		//| return {$$ = mknode($1, NULL, "");}
;

nameVoidBody: 	O_BRACE commands C_BRACE {$$ = mknode(NULL, $2, "BLOCK");};

typeFuncBody: 	O_BRACE commands return C_BRACE {$$ = mknode($2, $3, "BLOCK");};

funcNameVars: id O_PARENTH inputVars C_PARENTH {$$ = mknode($1, $3, "");};

inputVars:	{$$ = mknode(NULL, NULL, "");}
		| type id {$$ = mknode($1, $2, "NEW");}
		| inputVars COMMA inputVars {$$ = mknode($1, $3, "");}
;

return: /*RETURN SEMICOLON {$$ = mknode(NULL, NULL, "RETURN");}
	|*/ RETURN expr SEMICOLON {$$ = mknode(NULL, $2, "RETURN");}
	| RETURN boolExpr SEMICOLON {$$ = mknode(NULL, $2, "RETURN");}
;

/*returnVoid: 	| RETURN SEMICOLON {$$ = mknode(NULL, NULL, "RETURN");};*/

//funcCommands: commands | return;

/*
command: DIR_INCLUDE include {printf("directive\n"); $$ = mknode(NULL, $2, "INCLUDE");}
	| function funcBody {printf("function funcBody\n"); $$ = mknode(NULL, $2, "FUNC");}
;

include: LESS LIBRARY GREATER  {printf("LIBRARY\n"); $$ = mknode(NULL, NULL, "LIBRARY");}
	| CHAR_CONSTANT {printf("CHAR_CONSTANT\n"); $$ = mknode(NULL, NULL, yytext);}
;
*/
//function: ;
//funcBody: ;

%%

node *mknode(node *left, node *right, char *token) {
	node *newNode = (node*)malloc(sizeof(node));
	char *newToken = (char*)malloc(sizeof(token) + 1);
	strcpy(newToken, token);
	newNode->left = left;
	newNode->right = right;
	newNode->token = newToken;

	return newNode;
}

void printTree(node *tree, int tab) {
	if (tree == NULL)
		return;

	if (strcmp(tree->token, "") != 0) {
		if (tree->left || tree->right) {
			printf("\n");
			for (int i = 0; i < tab; i++)
				printf("\t");
			printf("(");
		}
		
		printf("%s ", tree->token);	
	}

	if (tree->left)
		printTree(tree->left, strcmp(tree->token, "") != 0 ? tab + 1 : tab);
	if (tree->right)
		printTree(tree->right, strcmp(tree->token, "") != 0 ? tab + 1 : tab);

	if (strcmp(tree->token, "") != 0) {
		if (tree->right) {
			printf("\n");
			for (int i = 0; i < tab; i++)
				printf("\t");
			printf(")\n");
		}
	}
}

void addSymbol(node *tree, int lvl) {
	if (lastSymbol >= symbolTableSize) {
		symbolTableSize += SIZE_ADD;
		symbolTable = realloc(symbolTable, sizeof(symbolTableStruct) * SIZE_ADD);
	}	

	char *tmp;
	symbolTable[lastSymbol].type = tree->left->token;
	symbolTable[lastSymbol].isFunc = 0;
	symbolTable[lastSymbol].level = lvl;
	//printf("type: %s\n", tree->left->token);

	if (strcmp(tree->right->token, "=") == 0) {
		tmp = tree->right->left->token;
		if (symbolNumber(tmp, lvl, 0) >= 0) {
			errors++;
			errorPrint(1, tmp);
			return;
		}
		symbolTable[lastSymbol].id = tree->right->left->token;
		//printf("id: %s\n", tree->right->left->token);
	} else
		if (strcmp(tree->right->token, "FUNC") == 0) {
			tmp = tree->right->left->left->token;
			if (symbolNumber(tmp, lvl, 1) >= 0) {
				errors++;
				errorPrint(1, tmp);
				return;
			}
			symbolTable[lastSymbol].isFunc = 1;
			symbolTable[lastSymbol].id = tmp;
			symbolTable[lastSymbol].funcAddress = tree->right->left;
			//printf("id: %s\n", tree->right->left->left->token);
		} else {
			tmp = tree->right->token;
			if (symbolNumber(tmp, lvl, 0) >= 0) {
				errors++;
				errorPrint(1, tmp);
				return;
			}
			symbolTable[lastSymbol].id = tmp;
			//printf("id: %s\n", tree->right->token);
		}

	lastSymbol++;
}

int symbolNumber(char *id, int lvl, int isFunc) {
	for (int i = 0; i < lastSymbol; i++) {
		if (symbolTable[i].level <= lvl && strcmp(symbolTable[i].id, id) == 0 && symbolTable[i].isFunc == isFunc)
			return i;
	}

	return -1;
}

void printSymbolTable() {
	for (int i = 0; i < lastSymbol; i++) {
		if (strcmp(symbolTable[i].id, "") != 0)
			printf("type: %s  id: %s isFunc: %d lvl: %d\n", symbolTable[i].type, symbolTable[i].id, symbolTable[i].isFunc, symbolTable[i].level);
	}
}

void cleanTableLvl(int lvl) {
	for (int i = 0; i < lastSymbol; i++) {
		if (symbolTable[i].level == lvl) {
			symbolTable[i].type = "";
			symbolTable[i].id = "";
			symbolTable[i].value = "";
			symbolTable[i].isFunc = 0;
			symbolTable[i].level = -1;
			symbolTable[i].funcAddress = NULL;
		}
	}
}

void startCheckTree(node *tree, int lvl) {
	symbolTable = malloc(sizeof(symbolTableStruct) * SIZE_ADD);
	root = tree;
	checkTree(tree, lvl);
	//printSymbolTable();
	int mainNum = symbolNumber("main", 0, 1);
	if (mainNum == -1)
		errorPrint(2, "main");

	if (checkFunc(symbolTable[mainNum].funcAddress) != 0)
		errorPrint(3, "main");

	//checkFunc(NULL, "calc");
}

int checkFunc(node *tree) {

	if (!tree->right->left && !tree->right->right)
		return 0;
	else
		return 1;
	
	
}

/*node *findFuncDeclar(node *tree, char *id) {
	if (tree == NULL)
		return NULL;
	
	node *tmp;
	printf("1");
	if (strcmp(tree->token, "") != 0) {
		printf("2");
		if (strcmp(tree->token, "NEW") == 0 && tree->right) {
			printf("3");
			if (strcmp(tree->right->token, "FUNC") == 0 && tree->right->left) {
				printf("4");
				tmp = tree->right;
				if (strcmp(tmp->left->left->token, id) == 0) {
					printf("5");
					tmp = tmp->left->left;
					return tmp;
				}
			}
		}
	}

	if (tree->left)
		return findFuncDeclar(tree->left, id);
	if (tree->right)
		return findFuncDeclar(tree->right, id);

	return NULL;
}*/

/*char *tokensVal(node *tree, char *token) {
	char *
}*/

void checkTree(node *tree, int lvl) {
	if (tree == NULL/* || errors > 0*/)
		return;

	char *tmp;
	int symbolNum = -1;


	if (strcmp(tree->token, "NEW") == 0) {
		addSymbol(tree, lvl);
	}

	if (strcmp(tree->token, "METHOD") == 0) {
		tmp = tree->left->token;
		symbolNum = symbolNumber(tmp, lvl, 1);
		if (symbolNum == -1)
			errorPrint(2, tmp);
		else {
			if (checkFunc(symbolTable[symbolNum].funcAddress) != checkFunc(tree))
				errorPrint(3, tmp);
		}
	}

	if (strcmp(tree->token, "=") == 0) {
		tmp = tree->left->token;
		symbolNum = symbolNumber(tmp, lvl, 0);
		if (symbolNum == -1)
			errorPrint(4, tmp);
		else {
			if (strcmp(tree->right->token, "ID") == 0) {
				char *tmpR = tree->right->right->token;
				int num = symbolNumber(tmpR, lvl, 0);
				if (num == -1)
					errorPrint(4, tmpR);
				//if (strcmp(symbolTable[num].value))
				if (strcmp(symbolTable[symbolNum].type, symbolTable[num].type) != 0)
					errorPrint(5, tmpR);
			} else if (strcmp(tree->right->token, "CONSTANT") == 0) {
				char *tmpR = tree->right->right->token;
				//int num = symbolNumber(tmpR, lvl, 0);
				//if (num == -1)
				//	errorPrint(4, tmpR);
				if (strcmp(symbolTable[symbolNum].type, tmpR) != 0)
					errorPrint(6, tmpR);
			}
		}
	}

	if (strcmp(tree->token, "+") == 0 || strcmp(tree->token, "-") == 0 || strcmp(tree->token, "*") == 0 || strcmp(tree->token, "/") == 0 || strcmp(tree->token, ">") == 0 || strcmp(tree->token, "<") == 0 || strcmp(tree->token, ">=") == 0 || strcmp(tree->token, "<=") == 0) {
		tmp = tree->left->right->token;
		symbolNum = symbolNumber(tmp, lvl, 0);
		if (strcmp(tree->left->token, "CONSTANT") == 0 && strcmp(tmp, "INT") != 0) {
			errorPrint(6, tmp);
		} else if (strcmp(tmp, "INT") != 0 && symbolNum == -1)
			errorPrint(4, tmp);
		else {
			if (strcmp(tmp, "INT") != 0 && strcmp(symbolTable[symbolNum].type, "INT") != 0)
				errorPrint(5, tmp);
		}

		tmp = tree->right->right->token;
		symbolNum = symbolNumber(tmp, lvl, 0);
		if (strcmp(tree->right->token, "CONSTANT") == 0 && strcmp(tmp, "INT") != 0) {
			errorPrint(6, tmp);
		} else if (strcmp(tmp, "INT") != 0 && symbolNum == -1)
			errorPrint(4, tmp);
		else {
			if (strcmp(tmp, "INT") != 0 && strcmp(symbolTable[symbolNum].type, "INT") != 0)
				errorPrint(5, tmp);
		}
		//printf("%s\n", tree->right->right->token);
	}

	if (strcmp(tree->token, "RETURN") == 0) {
		
		int i = lastSymbol - 1; 
		while (symbolTable[i].isFunc != 1) {
			i--;
		}
		char *funcType = symbolTable[i].type;
			
		if (strcmp(tree->right->token, "CONSTANT") == 0 && strcmp(tree->right->right->token, funcType) != 0) {
			errorPrint(6, tree->right->right->token);
		} else if (strcmp(tree->right->token, "ID") == 0) {// strcmp(tree->right->right->token, funcType) != 0)
			symbolNum = symbolNumber(tree->right->right->token, lvl, 0);
			if (symbolNum == -1)
				errorPrint(4, tree->right->right->token);
			else {
				if (strcmp(funcType, symbolTable[symbolNum].type) != 0)
					errorPrint(5, tree->right->right->token);
			}
		} else if (strcmp(funcType, "INT") != 0)
			errorPrint(6, funcType);

		//printf("Func type %s\n", tree->right->right->token);
	}

	if (strcmp(tree->token, "FUNC") == 0 || strcmp(tree->token, "FOR") == 0 || strcmp(tree->token, "WHILE") == 0 || strcmp(tree->token, "IF") == 0) {
		//printf("block\n");
		checkTree(tree->left, lvl + 1);
		checkTree(tree->right, lvl + 1);
		cleanTableLvl(lvl + 1);
		//return;
	} else {
		if (tree->left)
			checkTree(tree->left, lvl);
		if (tree->right)
			checkTree(tree->right, lvl);
	}	
	
	/*if (tree->left)
		checkTree(tree->left, lvl);
	if (tree->right)
		checkTree(tree->right, lvl);*/
}

void errorPrint(int errorCode, char *msg) {
	printf("Error: ");
	switch (errorCode) {
		case 1:
			printf("Multiplicate diclaration \"%s\"\n", msg);
			break;
		case 2:
			printf("Method \"%s\" is not declared\n", msg);
			break;
		case 3:
			printf("Incorrect number of parameters (Method \"%s\")\n", msg);
			break;
		case 4:
			printf("Variable \"%s\" is not declared\n", msg);
			break;
		case 5:
			printf("Incorrect type (Variable \"%s\")\n", msg);
			break;
		case 6:
			printf("Incorrect type (%s_CONSTANT)\n", msg);
			break;
	}
}

int yywrap() {
	return 1;
}

int main() {
	yyparse();
}
