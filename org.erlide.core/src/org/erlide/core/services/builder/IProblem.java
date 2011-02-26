/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation
 ****************************************************************************/
package org.erlide.core.services.builder;

/**
 * Description of a Erlang problem, as detected by the compiler or some of the
 * underlying technology reusing the compiler. A problem provides access to:
 * <ul>
 * <li>its location (originating source file name, source position, line
 * number),</li>
 * <li>its message description and a predicate to check its severity (warning or
 * error).</li>
 * <li>its id : an number identifying the very nature of this problem. All
 * possible IDs are listed as constants on this interface.</li>
 * </ul>
 * 
 * Note: the compiler produces IProblems internally, which are turned into
 * markers by the ErlangBuilder so as to persist problem descriptions. This
 * explains why there is no API allowing to reach IProblem detected when
 * compiling. However, the Erlang problem markers carry equivalent information
 * to IProblem, in particular their id (attribute "id") is set to one of the IDs
 * defined on this interface.
 * 
 */
public interface IProblem {

    /**
     * Answer back the original arguments recorded into the problem.
     * 
     * @return the original arguments recorded into the problem
     */
    String[] getArguments();

    /**
     * Returns the problem id
     * 
     * @return the problem id
     */
    int getID();

    /**
     * Answer a localized, human-readable message string which describes the
     * problem.
     * 
     * @return a localized, human-readable message string which describes the
     *         problem
     */
    String getMessage();

    /**
     * Answer the file name in which the problem was found.
     * 
     * @return the file name in which the problem was found
     */
    char[] getOriginatingFileName();

    /**
     * Answer the end position of the problem (inclusive), or -1 if unknown.
     * 
     * @return the end position of the problem (inclusive), or -1 if unknown
     */
    int getSourceEnd();

    /**
     * Answer the line number in source where the problem begins.
     * 
     * @return the line number in source where the problem begins
     */
    int getSourceLineNumber();

    /**
     * Answer the start position of the problem (inclusive), or -1 if unknown.
     * 
     * @return the start position of the problem (inclusive), or -1 if unknown
     */
    int getSourceStart();

    /**
     * Checks the severity to see if the Error bit is set.
     * 
     * @return true if the Error bit is set for the severity, false otherwise
     */
    boolean isError();

    /**
     * Checks the severity to see if the Error bit is not set.
     * 
     * @return true if the Error bit is not set for the severity, false
     *         otherwise
     */
    boolean isWarning();

    /**
     * Set the end position of the problem (inclusive), or -1 if unknown. Used
     * for shifting problem positions.
     * 
     * @param sourceEnd
     *            the given end position
     */
    void setSourceEnd(int sourceEnd);

    /**
     * Set the line number in source where the problem begins.
     * 
     * @param lineNumber
     *            the given line number
     */
    void setSourceLineNumber(int lineNumber);

    /**
     * Set the start position of the problem (inclusive), or -1 if unknown. Used
     * for shifting problem positions.
     * 
     * @param sourceStart
     *            the given start position
     */
    void setSourceStart(int sourceStart);

    /**
     * Problem Categories The high bits of a problem ID contains information
     * about the category of a problem. For example, (problemID & TypeRelated)
     * != 0, indicates that this problem is type related.
     * 
     * A problem category can help to implement custom problem filters. Indeed,
     * when numerous problems are listed, focusing on import related problems
     * first might be relevant.
     * 
     * When a problem is tagged as Internal, it means that no change other than
     * a local source code change can fix the corresponding problem.
     */
    int ModuleRelated = 0x01000000;

    int FunctionRelated = 0x02000000;

    int Internal = 0x04000000;

    int Syntax = 0x08000000;

    int Edoc = 0x10000000;

    /**
     * Mask to use in order to filter out the category portion of the problem
     * ID.
     */
    int IgnoreCategoriesMask = 0xFFFFFF;

    /**
     * Below are listed all available problem IDs. Note that this list could be
     * augmented in the future, as new features are added to the Erlang core
     * implementation.
     */

    /**
     * ID reserved for referencing an internal error inside the ErlangCore
     * implementation which may be surfaced as a problem associated with the
     * compilation unit which caused it to occur.
     */
    int Unclassified = 0;

    int ModuleNameMismatch = ModuleRelated + 1;

    // variables
    int UndefinedName = Internal + 50;

    int UninitializedLocalVariable = Internal + 51;

    // local variables
    int RedefinedLocal = Internal + 55;

    // functions
    int UndefinedFunction = FunctionRelated + 70;

    // variable hiding
    /**
     * The local variable {0} is hiding another local variable defined in an
     * enclosing type scope
     */
    int ShadowedVariable = Internal + 90;

    // methods
    int NotExportedFunction = FunctionRelated + 101;

    // miscellaneous
    int ParsingError = Syntax + Internal + 204;

    // syntax errors
    int UnmatchedBracket = Syntax + Internal + 220;

    int MissingSemiColon = Syntax + Internal + 224;

    int ParsingErrorUnexpectedEOF = Syntax + Internal + 239;

    // scanner errors
    int EndOfSource = Syntax + Internal + 250;

    int InvalidHexa = Syntax + Internal + 251;

    int InvalidOctal = Syntax + Internal + 252;

    int InvalidCharacterConstant = Syntax + Internal + 253;

    int InvalidEscape = Syntax + Internal + 254;

    int InvalidInput = Syntax + Internal + 255;

    int InvalidUnicodeEscape = Syntax + Internal + 256;

    int InvalidFloat = Syntax + Internal + 257;

    int NullSourceString = Syntax + Internal + 258;

    int UnterminatedString = Syntax + Internal + 259;

    int UnterminatedComment = Syntax + Internal + 260;

    // method related problems
    int DuplicateFunction = FunctionRelated + 355;

    // detected task
    int Task = Internal + 450;

}
