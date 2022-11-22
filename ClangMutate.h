#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>
#include <ctime>
#include <cstdlib>
#include <random>

namespace clang {
namespace mamba {

using namespace llvm;
using namespace clang::ast_matchers;
// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static cl::OptionCategory MambaCov("clang-mutate options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
// static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
									
static cl::opt<bool> OptionChangeflow("changeflow",
									cl::desc("change control flow and data flow"),
									cl::init(false),cl::cat(MambaCov),
									cl::Optional);

static cl::opt<bool> OptionTruecode("true",
                      cl::desc("construct true block code"),
                      cl::init(0), cl::cat(MambaCov), cl::Optional);

static cl::opt<bool> OptionFalsecode("false",
                      cl::desc("construct false block code"),
                      cl::init(0), cl::cat(MambaCov), cl::Optional);

static cl::opt<int> OptionStart("start",
                      cl::desc("set the start position of the compoundStmt"),
                      cl::init(0), cl::cat(MambaCov), cl::Optional);

static cl::opt<int> OptionEnd("end",
                      cl::desc("set the start position of the compoundStm"),
                      cl::init(0), cl::cat(MambaCov), cl::Optional);


// A help message for this specific tool can be added afterwards.

static cl::extrahelp MoreHelp("\n\t this is wrote by heng \n");

uint32_t generateRandom(uint32_t begin,uint32_t end)
{
	if(begin>end)
		begin=end;

    std::random_device rd;

	return (rd()%(end-begin+1)+begin);
}


} // namespace mamba

} // namespace clang
