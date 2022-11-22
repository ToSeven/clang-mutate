#include "ClangMutate.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang::mamba;
using namespace llvm;
using namespace std;

class TrueFalseCodeHander : public MatchFinder::MatchCallback {
 public:
  explicit TrueFalseCodeHander(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    const VarDecl *NodeTrue = Result.Nodes.getNodeAs<VarDecl>("var_true");
    if (NodeTrue && NodeTrue->getNameAsString().find("true_") != string::npos)
      VarTrueName.push_back(NodeTrue->getNameAsString());

    const VarDecl *NodeFalse = Result.Nodes.getNodeAs<VarDecl>("var_false");
    if (NodeFalse &&
        NodeFalse->getNameAsString().find("false_") != string::npos)
      VarFalseName.push_back(NodeFalse->getNameAsString());

    const CompoundStmt *NodeComp =
        Result.Nodes.getNodeAs<CompoundStmt>("compound");

    if(NodeComp)
    {
        /* SourceRange NodeRange = NodeComp->getSourceRange(); */
        /* NodeComp->dumpColor(); */
        /* std::cout<<Result.SourceManager->getPresumedLineNumber(NodeComp->getRBracLoc())<<std::endl; */
        SourceLocation BeginLoc = NodeComp->getBeginLoc();
        SourceLocation EndLoc = NodeComp->getEndLoc();

        uint32_t CompoundRange = Result.SourceManager->getPresumedLineNumber(EndLoc) -
                                 Result.SourceManager->getPresumedLineNumber(BeginLoc);
        
        if(CompoundRange != 0)
        {
            CompoundResult.push_back(Result);
            /* NodeComp->dumpColor(); */
        }
    }

    /* if (NodeComp && Result.SourceManager->getPresumedLineNumber( */
    /*                     NodeComp->getLBracLoc()) == OptionStart) */
    /*   CompoundResult.push_back(Result); */

  }

  virtual void onEndOfTranslationUnit() {
    uint32_t RandomNum = 0;
    string VarName = "";

    if (OptionTruecode) {
      RandomNum = generateRandom(0, VarTrueName.size() - 1);
      VarName = VarTrueName[RandomNum];
    }

    if (OptionFalsecode) {
      RandomNum = generateRandom(0, VarFalseName.size() - 1);
      VarName = VarFalseName[RandomNum];
    }

    if (VarName == "") {
      std::cout << "varname error !!!";
      return;
    }

    RandomNum=generateRandom(0,CompoundResult.size()-1);
    auto CompoundSelected = CompoundResult[RandomNum];
    const CompoundStmt *NodeComp =
        CompoundSelected.Nodes.getNodeAs<CompoundStmt>("compound");

    SourceManager *NodeSourceManage = CompoundSelected.SourceManager;

    /* SourceLocation BeginLoc = NodeComp->getBeginLoc(); */
    /* SourceLocation EndLoc = NodeComp->getEndLoc(); */

    /* uint32_t CompoundRange = NodeSourceManage->getPresumedLineNumber(EndLoc) - */
    /*                          NodeSourceManage->getPresumedLineNumber(BeginLoc); */
    /* std::cout<<CompoundRange<<std::endl; */
    /* std::cout << CompoundRange<<std::endl; */
    /* std::cout << NodeSourceManage->getPresumedColumnNumber(EndLoc) <<
     * std::endl; */
    /* std::cout << "nodesize: " << NodeComp->size() << std::endl; */

    uint32_t NodeSize = NodeComp->size();

    auto *NodeStart = NodeComp->body_begin();

    SourceRange NodeRange = (*NodeStart)->getSourceRange();

    string space = Lexer::getIndentationForLine((*NodeStart)->getBeginLoc(),
                                                *NodeSourceManage)
                       .str();

    string JudgeStatement = "if( " + VarName + " > 0 " + ")\n" + space +"{\n" +space;

    if (NodeSize == 0) {
      std::cout << "nodesize error !!!";
      return;
    }

    if (NodeSize == 1) {
            if (OptionTruecode) {
        while (1) {
          SourceLocation temp = clang::Lexer::getLocForEndOfToken(
              NodeRange.getEnd(), 0, *NodeSourceManage, LangOptions());

          if (temp.printToString(*NodeSourceManage) ==
              NodeRange.getEnd().printToString(*NodeSourceManage))
            break;

          // std::cout<<"temp:"<<temp.printToString(*NodeSourceManage)<<std::endl;
          // std::cout<<"
          // noderange"<<NodeRange.getEnd().printToString(*NodeSourceManage)<<std::endl;

          NodeRange.setEnd(temp);
        }
        Rewrite.InsertTextAfter((*NodeStart)->getBeginLoc(), JudgeStatement);
        Rewrite.InsertTextAfter(NodeRange.getEnd(), "\n" + space + "}");

        std::cout << NodeSourceManage->getPresumedLineNumber(
                         (*NodeStart)->getBeginLoc())
                  << " "
                  << NodeSourceManage->getPresumedLineNumber(
                         NodeRange.getEnd()) +
                         3;
      } else {
        NodeRange = (*NodeStart)->getSourceRange();
        while (1) {
          SourceLocation temp = clang::Lexer::getLocForEndOfToken(
              NodeRange.getEnd(), 0, *NodeSourceManage, LangOptions());
          if (temp.printToString(*NodeSourceManage) ==
              NodeRange.getEnd().printToString(*NodeSourceManage))
            break;
          NodeRange.setEnd(temp);
        }

        StringRef ref =
            Lexer::getSourceText(CharSourceRange::getCharRange(NodeRange),
                                 *NodeSourceManage, LangOptions());
        string insert_statement = ref.str();
        insert_statement += "\n" + space + "}\n"+space;
        Rewrite.InsertTextBefore((*NodeStart)->getBeginLoc(), insert_statement);
        Rewrite.InsertTextBefore((*NodeStart)->getBeginLoc(), JudgeStatement);

        uint32_t range =
            NodeSourceManage->getPresumedLineNumber((*NodeStart)->getEndLoc()) -
            NodeSourceManage->getPresumedLineNumber(
                (*NodeStart)->getBeginLoc());

        std::cout << NodeSourceManage->getPresumedLineNumber(
                         (*(NodeStart))->getBeginLoc())
                  << " "
                  << NodeSourceManage->getPresumedLineNumber(
                         (*(NodeStart))->getBeginLoc()) +
                         range + 3;
      }
    } else {
      if (OptionTruecode) {
        uint32_t InsertHeadPosition = generateRandom(0, NodeSize - 1);
        uint32_t InsertTailPosition =
            generateRandom(InsertHeadPosition, NodeSize - 1);
        auto NodeHead = NodeStart + InsertHeadPosition;
        auto NodeTail = NodeStart + InsertTailPosition;

        /* string IfStatement = "if(*" + VarName + ")\n" + space + "{\n" +
         * space;
         */
        string comp_statement = "\n" + space + "}";

        Rewrite.InsertTextAfter((*(NodeHead))->getBeginLoc(), JudgeStatement);

        NodeRange = (*NodeTail)->getSourceRange();
        while (1) {
          SourceLocation temp = clang::Lexer::getLocForEndOfToken(
              NodeRange.getEnd(), 0, *NodeSourceManage, LangOptions());
          if (temp.printToString(*NodeSourceManage) ==
              NodeRange.getEnd().printToString(*NodeSourceManage))
            break;
          NodeRange.setEnd(temp);
        }
        Rewrite.InsertTextAfter(NodeRange.getEnd(), comp_statement);

        std::cout << NodeSourceManage->getPresumedLineNumber(
                         (*(NodeHead))->getBeginLoc())
                  << " "
                  << NodeSourceManage->getPresumedLineNumber(
                         NodeRange.getEnd()) +
                         3;
      } else {
        uint32_t InsertHeadPosition = generateRandom(0, NodeSize - 1);
        uint32_t InsertTailPosition =
            generateRandom(InsertHeadPosition, NodeSize - 1);
        auto NodeHead = NodeStart + InsertHeadPosition;
        auto NodeTail = NodeStart + InsertTailPosition;

        string CopyStatement = "";
        /* int IntNodeRange=0; */
        /* int SumRange=0; */

        for (auto StmtNode = NodeHead; StmtNode <= NodeTail; StmtNode++) {
          /* (*(StmtNode))->dumpColor(); */
          CopyStatement += getSourceStatement(StmtNode, NodeSourceManage);
          /* std::cout<<"range: "<<IntNodeRange<< "str: "<<CopyStatement<<std::endl; */
          /* SumRange+=IntNodeRange; */
          /* std::cout<<CopyStatement<<std::endl; */
          CopyStatement += "\n" + space;
        }

        string CompStatement ="}\n"+space;
        Rewrite.InsertTextBefore((*(NodeHead))->getBeginLoc(), CompStatement);
        Rewrite.InsertTextBefore((*(NodeHead))->getBeginLoc(), CopyStatement);
        Rewrite.InsertTextBefore((*(NodeHead))->getBeginLoc(), JudgeStatement);

        /* NodeRange = (*NodeTail)->getSourceRange(); */
        /* while (1) { */
        /*   SourceLocation temp = clang::Lexer::getLocForEndOfToken( */
        /*       NodeRange.getEnd(), 0, *NodeSourceManage, LangOptions()); */
        /*   if (temp.printToString(*NodeSourceManage) == */
        /*       NodeRange.getEnd().printToString(*NodeSourceManage)) */
        /*     break; */
        /*   NodeRange.setEnd(temp); */
        /* } */
        // Rewrite.InsertTextAfter(NodeRange.getEnd(), comp_statement);


        /* int range = NodeSourceManage->getPresumedLineNumber(NodeRange.getEnd())-NodeSourceManage->getPresumedLineNumber((*NodeHead)->getBeginLoc()); */

        std::cout << NodeSourceManage->getPresumedLineNumber(
                         (*(NodeHead))->getBeginLoc())
                  << " "
                   /* << (NodeSourceManage->getPresumedLineNumber((*(NodeHead))->getBeginLoc())+range+3); */
                  /* << NodeSourceManage->getPresumedLineNumber(NodeRange.getEnd())+3; */
                  << NodeSourceManage->getPresumedLineNumber((*NodeTail)->getEndLoc())+3;
        }
    }
    Rewrite.overwriteChangedFiles();
  }
 private:
  string getSourceStatement(const Stmt *const *StmtNode,
                       SourceManager *NodeSourceManage) {
    SourceRange NodeRange = (*StmtNode)->getSourceRange();
    while (1) {
      SourceLocation temp = clang::Lexer::getLocForEndOfToken(
          NodeRange.getEnd(), 0, *NodeSourceManage, LangOptions());
      if (temp.printToString(*NodeSourceManage) ==
          NodeRange.getEnd().printToString(*NodeSourceManage))
        break;
      NodeRange.setEnd(temp);
    }
    string ref =
        Lexer::getSourceText(CharSourceRange::getCharRange(NodeRange),
                             *NodeSourceManage, LangOptions()).str();
    // std::cout<<ref<<std::endl;
    /* NumNodeRange = NodeSourceManage->getPresumedLineNumber(NodeRange.getEnd())- NodeSourceManage->getPresumedLineNumber(NodeRange.getBegin())+1; */
    return ref;
  }

  Rewriter &Rewrite [[maybe_unused]];
  vector<string> VarTrueName;
  vector<string> VarFalseName;
  vector<MatchFinder::MatchResult> CompoundResult;
  vector<MatchFinder::MatchResult> VarintResult;
  vector<MatchFinder::MatchResult> StmtResult;
  vector<MatchFinder::MatchResult> FunctionResult;
};

class MambaASTConsumer : public ASTConsumer {
 public:
  explicit MambaASTConsumer(Rewriter &R) : HandlerTrueFalse(R) {
    DeclarationMatcher TrueVarMatchint =
        varDecl(hasGlobalStorage()).bind("var_true");
    DeclarationMatcher FalseVarMatchint =
        varDecl(hasGlobalStorage()).bind("var_false");

    StatementMatcher CompoundMatch =
        compoundStmt(isExpansionInMainFile()).bind("compound");

    Matcher.addDynamicMatcher(TrueVarMatchint, &HandlerTrueFalse);
    Matcher.addDynamicMatcher(FalseVarMatchint, &HandlerTrueFalse);
    Matcher.addDynamicMatcher(CompoundMatch, &HandlerTrueFalse);
  }
  void HandleTranslationUnit(ASTContext &Context) override {
    Matcher.matchAST(Context);
  }

 private:
  TrueFalseCodeHander HandlerTrueFalse;
  MatchFinder Matcher;
};

class MambaFrontendAction : public ASTFrontendAction {
 public:
  MambaFrontendAction(){};
  ~MambaFrontendAction(){};

  int CopyFile(char *SourceFile, char *NewFile) {
    ifstream in;
    ofstream out;
    in.open(SourceFile, ios::binary);  //
    if (in.fail())                     //
    {
      cout << "Error 1: Fail to open the source file." << endl;
      in.close();
      out.close();
      return 0;
    }
    out.open(NewFile, ios::binary);  //
    if (out.fail())                  //
    {
      cout << "Error 2: Fail to create the new file." << endl;
      out.close();
      in.close();
      return 0;
    } else  //
    {
      out << in.rdbuf();
      out.close();
      in.close();
      return 1;
    }
  }

  void EndSourceFileAction() {
    // SourceManager &SM = TheRewriter.getSourceMgr();
    // llvm::errs() << "** EndSourceFileAction for: "
    //  << SM.getFileEntryForID(SM.getMainFileID())->getName() << "\n";

    // Now emit the rewritten buffer.
    // CopyFile("main.c", "mainori.c");
    // TheRewriter.overwriteChangedFiles();
    // rename("main.c", "mainvar.c");
    // rename("mainori.c", "main.c");
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    // llvm::errs() << "** Creating AST consumer for: " << file << "\n";
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<MambaASTConsumer>(TheRewriter);
  }

 private:
  Rewriter TheRewriter;
};

class MambaASTDumper : public ASTFrontendAction {
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    StringRef temp;
    return clang::CreateASTDumper(nullptr /*Dump to stdout.*/, temp,
                                  /*DumpDecls=*/true,
                                  /*Deserialize=*/false,
                                  /*DumpLookups=*/false,
                                  /*DumpDeclTypes=*/false, clang::ADOF_Default);
  }
};

int main(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, MambaCov);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
   /* Tool.run(newFrontendActionFactory<MambaASTDumper>().get()); */
  return Tool.run(newFrontendActionFactory<MambaFrontendAction>().get());
}
