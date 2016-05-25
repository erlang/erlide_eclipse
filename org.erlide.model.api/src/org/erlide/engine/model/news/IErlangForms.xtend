package org.erlide.engine.model.news

import org.eclipse.jdt.annotation.NonNull
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
    @NonNull
    def String getTag()
}

interface IErlangFunction extends IErlangForm {
    @NonNull
    def String getName()

    def int getArity()

    @NonNull
    def Iterable<IErlangFunctionClause> getClauses()

    def IErlangComment getComment()
}

interface IErlangComment extends IErlangForm {
    /**
     * Comments on the same level on succesive lines are merged together.
     * %-signs are not included in the text.
     */
    @NonNull
    def Iterable<String> getText()

    /**
     * The number of % signs in front of the comment
     */
    def int getLevel()
}

interface IErlangError extends IErlangForm, IErlangExpression {
    @NonNull
    def String getMessage()
}

interface IErlangModuleDef extends IErlangAttribute {
}

interface IErlangRecordDef extends IErlangAttribute {
    @NonNull
    def String getName()

    @NonNull
    def IErlangExpression getDefinition()
}

interface IErlangPreprocessor extends IErlangAttribute {
}

interface IErlangDefine extends IErlangPreprocessor {
    @NonNull
    def String getName()

    def IErlangExpression getValue()
}

interface IErlangTypeDef extends IErlangAttribute {
    @NonNull
    def String getName()

    @NonNull
    def IErlangExpression getDefinition()
}

interface IErlangOpaqueDef extends IErlangAttribute {
    @NonNull
    def IErlangExpression getDefinition()
}

interface IErlangExport extends IErlangAttribute {
    @NonNull
    def Iterable<IErlangFunctionRef> getFunctions()
}

interface IErlangImport extends IErlangAttribute {
    @NonNull
    def Iterable<IErlangFunctionRef> getFunctionRefs()
}

interface IErlangInclude extends IErlangPreprocessor {
    @NonNull
    def String getFileName()
}

interface IErlangCompilerOpts extends IErlangAttribute {
    @NonNull
    def IErlangExpression getValue()
}

interface IErlangIf extends IErlangPreprocessor {
    @NonNull
    def String getCondition()

    @NonNull
    def Iterable<IErlangForm> getIfForms()

    @NonNull
    def Iterable<IErlangForm> getElseForms()
}

interface IErlangUndef extends IErlangPreprocessor {
    @NonNull
    def String getName()
}

interface IErlangBehaviour extends IErlangAttribute {
    @NonNull
    def String getName()
}

interface IErlangCallback extends IErlangAttribute {
}

interface IErlangTypeSpec extends IErlangAttribute {
    // Returns a ref because the function may be from a different source file
    @NonNull
    def IErlangFunctionRef getFunction()

    @NonNull
    def IErlangExpression getSpec()
}

interface IErlangFunctionClause extends IErlangElement, ISourceConstruct {
    @NonNull
    def Iterable<IErlangExpression> getFormalParameters()

    def IErlangGuard getGuard()

    def IErlangExpression getBody()
}

interface IErlangExpression extends IErlangElement, ISourceConstruct {
    @NonNull
    def String getContent()
}

interface IErlangGuard extends IErlangExpression {
}

interface IErlangFunctionRef extends IErlangModuleRef {
    @NonNull
    def String getName()

    def int getArity()
}

interface IErlangModuleRef {
    @NonNull
    def String getModule()
}
