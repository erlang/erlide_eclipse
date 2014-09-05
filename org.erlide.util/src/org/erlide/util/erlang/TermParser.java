/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.util.erlang;

import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Strings;

public class TermParser {

    public OtpErlangObject parse(final String s) throws TermParserException {
        return doParse(s);
    }

    protected static OtpErlangObject doParse(final String s) throws TermParserException {
        if (Strings.isNullOrEmpty(s)) {
            return null;
        }
        return parse(scan(s));
    }

    private static OtpErlangObject parse(final List<Token> tokens)
            throws TermParserException {
        if (tokens.isEmpty()) {
            return null;
        }
        OtpErlangObject result = null;
        final Token t = tokens.remove(0);
        final String text = t.text;
        if (text == null) {
            throw new TermParserException("null token" + t.toString());
        }
        switch (t.kind) {
        case ATOM:
            result = new OtpErlangAtom(text);
            break;
        case VARIABLE:
            result = new OtpPatternVariable(text);
            break;
        case STRING:
            result = new OtpErlangString(text);
            break;
        case INTEGER:
            result = new OtpErlangLong(Long.parseLong(text));
            break;
        case PLACEHOLDER:
            result = new OtpFormatPlaceholder(text);
            break;
        case TUPLESTART:
            result = parseTuple(tokens, new Stack<OtpErlangObject>());
            break;
        case TUPLEEND:
            throw new TermParserException("unexpected " + t.toString());
        case LISTSTART:
            result = parseList(tokens, new Stack<OtpErlangObject>(), null);
            break;
        case LISTEND:
            throw new TermParserException("unexpected " + t.toString());
        case MAP:
            result = parseMap(tokens, new Stack<OtpErlangObject>());
            break;
        case COMMA:
            throw new TermParserException("unexpected " + t.toString());
        default:
            throw new TermParserException("unknown token" + t.toString());
        }
        return result;
    }

    private static OtpErlangObject parseList(final List<Token> tokens,
            final Stack<OtpErlangObject> stack, final OtpErlangObject tail)
            throws TermParserException {
        if (tokens.isEmpty()) {
            return null;
        }
        final Token t = tokens.get(0);
        if (t.kind == TokenKind.LISTEND) {
            tokens.remove(0);
            try {
                return new OtpErlangList(
                        stack.toArray(new OtpErlangObject[stack.size()]), tail);
            } catch (final OtpErlangException e) {
                ErlLogger.error(e);
                // can't happen
                return null;
            }
        }
        OtpErlangObject atail = tail;
        if (t.kind == TokenKind.CONS) {
            tokens.remove(0);
            atail = parse(tokens);
        } else {
            stack.push(parse(tokens));
            if (tokens.get(0).kind == TokenKind.COMMA) {
                tokens.remove(0);
            }
        }
        return parseList(tokens, stack, atail);
    }

    private static OtpErlangObject parseTuple(final List<Token> tokens,
            final Stack<OtpErlangObject> stack) throws TermParserException {
        if (tokens.isEmpty()) {
            return null;
        }
        final Token t = tokens.get(0);
        if (t.kind == TokenKind.TUPLEEND) {
            tokens.remove(0);
            return new OtpErlangTuple(stack.toArray(new OtpErlangObject[stack.size()]));
        }
        if (t.kind == TokenKind.CONS) {
            throw new TermParserException("cons is invalid in tuple");
        }
        stack.push(parse(tokens));
        if (tokens.get(0).kind == TokenKind.COMMA) {
            tokens.remove(0);
        }
        return parseTuple(tokens, stack);
    }

    private static OtpErlangObject parseMap(final List<Token> tokens,
            final Stack<OtpErlangObject> stack) throws TermParserException {
        if (tokens.isEmpty()) {
            return null;
        }
        final Token t = tokens.get(0);
        if (t.kind == TokenKind.TUPLEEND) {
            tokens.remove(0);
            final int size = stack.size();
            final OtpErlangObject[] all = stack.toArray(new OtpErlangObject[size]);
            final OtpErlangObject[] keys = new OtpErlangObject[size / 2];
            final OtpErlangObject[] values = new OtpErlangObject[size / 2];
            for (int i = 0; i < size / 2; i++) {
                keys[i] = all[i * 2];
                values[i] = all[i * 2 + 1];
            }
            return new OtpErlangMap(keys, values);
        }
        stack.push(parse(tokens));
        if (tokens.get(0).kind != TokenKind.ARROW) {
            throw new TermParserException("badly constructed map");
        }
        tokens.remove(0);
        stack.push(parse(tokens));
        if (tokens.get(0).kind == TokenKind.COMMA) {
            tokens.remove(0);
        }
        return parseMap(tokens, stack);
    }

    private static enum TokenKind {
        ATOM, VARIABLE, STRING, INTEGER, PLACEHOLDER, TUPLESTART, TUPLEEND, LISTSTART, LISTEND, COMMA, CONS, MAP, ARROW, UNKNOWN;
    }

    private static class Token {

        TokenKind kind;
        int start;
        int end;
        String text;

        @Override
        public String toString() {
            return "<" + kind.toString() + ": !" + text + "!>";
        }

        public static Token nextToken(final String s) {
            if (s == null || s.length() == 0) {
                return null;
            }
            final Token result = new Token();
            char c;
            int i = 0;
            do {
                c = s.charAt(i++);
                if (i >= s.length()) {
                    return null;
                }
            } while (c == ' ' || c == '\t' || c == '\n' || c == '\r');
            i--;

            result.start = i;
            result.end = i;
            if (c <= 'z' && c >= 'a') {
                scanAtom(s, result);
            } else if (c == '\'') {
                scanQAtom(s, result);
            } else if (c == '"') {
                scanString(s, result);
            } else if (c >= 'A' && c <= 'Z' || c == '_') {
                scanVariable(s, result);
            } else if (c <= '9' && c >= '0' || c == '-') {
                scanInteger(s, result);
            } else if (c == '~') {
                scanPlaceholder(s, result);
            } else if (c == '{') {
                result.kind = TokenKind.TUPLESTART;
                result.end = result.start + 1;
            } else if (c == '}') {
                result.kind = TokenKind.TUPLEEND;
                result.end = result.start + 1;
            } else if (c == '[') {
                result.kind = TokenKind.LISTSTART;
                result.end = result.start + 1;
            } else if (c == ']') {
                result.kind = TokenKind.LISTEND;
                result.end = result.start + 1;
            } else if (c == ',') {
                result.kind = TokenKind.COMMA;
                result.end = result.start + 1;
            } else if (c == '|') {
                result.kind = TokenKind.CONS;
                result.end = result.start + 1;
            } else if (c == '#' && s.charAt(i + 1) == '{') {
                result.kind = TokenKind.MAP;
                result.end = result.start + 2;
            } else if (c == '=' && s.charAt(i + 1) == '>') {
                result.kind = TokenKind.ARROW;
                result.end = result.start + 2;
            } else {
                result.kind = TokenKind.UNKNOWN;
                result.end = result.start + 1;
            }
            result.text = s.substring(result.start, result.end);
            final char ch = result.text.charAt(0);
            if (result.kind == TokenKind.STRING) {
                result.text = unescape(result.text);
            } else if (result.kind == TokenKind.PLACEHOLDER) {
                result.text = result.text.substring(1);
            } else if (result.kind == TokenKind.ATOM && ch == '\'') {
                result.text = result.text.substring(1, result.text.length() - 1);
            }

            return result;
        }

        private static void scanPlaceholder(final String s, final Token result) {
            result.kind = TokenKind.PLACEHOLDER;
            char c;
            c = s.charAt(++result.end);
            while (result.end <= s.length()
                    && (c >= 'a' && c <= 'z' || c >= '0' && c <= '9')) {
                c = s.charAt(result.end++);
            }
            result.end--;
        }

        private static void scanInteger(final String s, final Token result) {
            char c;
            c = s.charAt(result.end);
            result.kind = TokenKind.INTEGER;
            while (result.end < s.length() && (c >= '0' && c <= '9' || c == '-')) {
                c = s.charAt(result.end++);
            }
            result.end--;
        }

        private static void scanVariable(final String s, final Token result) {
            char c;
            c = s.charAt(result.end);
            result.kind = TokenKind.VARIABLE;
            while (result.end < s.length() && c >= 'a' && c <= 'z' || c >= 'A'
                    && c <= 'Z' || c >= '0' && c <= '9' || c == '_' || c == ':') {
                c = s.charAt(result.end++);
            }
            result.end--;
        }

        private static void scanString(final String s, final Token result) {
            char c;
            result.kind = TokenKind.STRING;
            c = s.charAt(++result.end);
            while (result.end < s.length() && c != '"') {
                if (c == '\\') {
                    c = s.charAt(result.end++);
                }
                c = s.charAt(result.end++);
            }
        }

        private static void scanQAtom(final String s, final Token result) {
            char c;
            result.kind = TokenKind.ATOM;
            c = s.charAt(++result.end);
            while (result.end < s.length() && c != '\'') {
                if (c == '\\') {
                    c = s.charAt(result.end++);
                }
                c = s.charAt(result.end++);
            }
        }

        private static void scanAtom(final String s, final Token result) {
            char c;
            c = s.charAt(result.end);
            result.kind = TokenKind.ATOM;
            while (result.end < s.length() && c >= 'a' && c <= 'z' || c >= 'A'
                    && c <= 'Z' || c >= '0' && c <= '9' || c == '_') {
                c = s.charAt(result.end++);
            }
            result.end--;
        }
    }

    private static List<Token> scan(final String s) {
        String ss = s + " ";
        final List<Token> result = new ArrayList<Token>();
        Token t = Token.nextToken(ss);
        while (t != null) {
            result.add(t);
            ss = ss.substring(t.end);
            t = Token.nextToken(ss);
        }
        return result;
    }

    private static String unescape(final String message) {
        final StreamTokenizer parser = new StreamTokenizer(new StringReader(message));
        String result;
        try {
            parser.nextToken();
            if (parser.ttype == '"') {
                result = parser.sval;
            } else {
                result = "ERROR!";
            }
        } catch (final IOException e) {
            result = e.toString();
        }
        return result;
    }

}
