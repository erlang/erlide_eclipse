package org.erlide.engine.model.news

import org.eclipse.xtend.lib.annotations.Data

interface IErlangForm extends IErlangElement, ISourceConstruct {
// API to interact with the erlang-based model?
}

interface ISourceConstruct {
    def TextRange getFullRange()

    def TextRange getIdentifyingRange()
}

// use Handly's
@Data
class TextRange {
    int offset
    int length
}

interface IErlangAttribute extends IErlangForm {
    def String getTag()
}

interface IErlangFunction extends IErlangForm {
    def String getName()

    def int getArity()

    def Iterable<IErlangFunctionClause> getClauses()

    def IErlangComment getComment()
}

interface IErlangComment extends IErlangForm {
    /**
     * Comments on the same level on succesive lines are merged together.
     * %-signs are not included in the text.
     */
    def Iterable<String> getText()

    /**
     * The number of % signs in front of the comment
     */
    def int getLevel()
}

interface IErlangError extends IErlangForm, IErlangExpression {
    
    def String getMessage()
}

interface IErlangModuleDef extends IErlangAttribute {
}

interface IErlangRecordDef extends IErlangAttribute {
    def String getName()

    def IErlangExpression getDefinition()
}

interface IErlangPreprocessor extends IErlangAttribute {
}

interface IErlangDefine extends IErlangPreprocessor {
    def String getName()

    def IErlangExpression getValue()
}

interface IErlangTypeDef extends IErlangAttribute {
    def String getName()

    def IErlangExpression getDefinition()
}

interface IErlangOpaqueDef extends IErlangAttribute {
    def IErlangExpression getDefinition()
}

interface IErlangExport extends IErlangAttribute {
    def Iterable<IErlangFunctionRef> getFunctions()
}

interface IErlangImport extends IErlangAttribute {
    def Iterable<IErlangFunctionRef> getFunctionRefs()
}

interface IErlangInclude extends IErlangPreprocessor {
    def String getFileName()
}

interface IErlangCompilerOpts extends IErlangAttribute {
    def IErlangExpression getValue()
}

interface IErlangIf extends IErlangPreprocessor {
    def String getCondition()

    def Iterable<IErlangForm> getIfForms()

    def Iterable<IErlangForm> getElseForms()
}

interface IErlangUndef extends IErlangPreprocessor {
    def String getName()
}

interface IErlangBehaviour extends IErlangAttribute {
    def String getName()
}

interface IErlangCallback extends IErlangAttribute {
}

interface IErlangTypeSpec extends IErlangAttribute {
    // Returns a ref because the function may be from a different source file
    def IErlangFunctionRef getFunction()

    def IErlangExpression getSpec()
}

interface IErlangFunctionClause extends IErlangElement, ISourceConstruct {
    def Iterable<IErlangExpression> getFormalParameters()

    def IErlangGuard getGuard()

    def IErlangExpression getBody()
}

interface IErlangExpression extends IErlangElement, ISourceConstruct {
    def String getContent()
}

interface IErlangGuard extends IErlangExpression {
}

interface IErlangFunctionRef extends IErlangModuleRef {
    def String getName()

    def int getArity()
}

interface IErlangModuleRef {
    def String getModule()
}
