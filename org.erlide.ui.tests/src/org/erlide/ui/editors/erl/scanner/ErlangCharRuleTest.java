package org.erlide.ui.editors.erl.scanner;

import static org.junit.Assert.*;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.junit.Test;

public class ErlangCharRuleTest {
    public class StringCharacterScanner implements ICharacterScanner {
        private final String content;
        private int index;

        public StringCharacterScanner(final String content) {
            this.content = content;
            index = 0;
        }

        public String getCurrentContent() {
            return content.substring(index);
        }

        public String getScannedContent() {
            return content.substring(0, index);
        }

        @Override
        public char[][] getLegalLineDelimiters() {
            return null;
        }

        @Override
        public int getColumn() {
            return 0;
        }

        @Override
        public int read() {
            if (index > content.length() - 1) {
                return -1;
            }
            return content.charAt(index++);
        }

        @Override
        public void unread() {
            index--;
        }

    }

    private final IToken token = new Token("test");
    StringCharacterScanner scanner;

    @Test
    public void noChar() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("aa");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(Token.UNDEFINED, tk);
        assertEquals("aa", scanner.getCurrentContent());
        assertEquals("", scanner.getScannedContent());
    }

    @Test
    public void simpleChar() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$z ");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals(" ", scanner.getCurrentContent());
        assertEquals("$z", scanner.getScannedContent());
    }

    @Test
    public void simpleChar_1() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$z");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("", scanner.getCurrentContent());
        assertEquals("$z", scanner.getScannedContent());
    }

    @Test
    public void escapedChar() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\b");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("", scanner.getCurrentContent());
        assertEquals("$\\b", scanner.getScannedContent());
    }

    @Test
    public void octal_1() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\123z ");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("z ", scanner.getCurrentContent());
        assertEquals("$\\123", scanner.getScannedContent());
    }

    @Test
    public void octal_2() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\1z23z ");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("z23z ", scanner.getCurrentContent());
        assertEquals("$\\1", scanner.getScannedContent());
    }

    @Test
    public void octal_3() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\12");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("", scanner.getCurrentContent());
        assertEquals("$\\12", scanner.getScannedContent());
    }

    @Test
    public void hex_1() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\x123z ");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("3z ", scanner.getCurrentContent());
        assertEquals("$\\x12", scanner.getScannedContent());
    }

    @Test
    public void hex_2() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\x{cFE1}23z ");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("23z ", scanner.getCurrentContent());
        assertEquals("$\\x{cFE1}", scanner.getScannedContent());
    }

    @Test
    public void hex_3() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\x{cFzE1}23z ");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("{cFzE1}23z ", scanner.getCurrentContent());
        assertEquals("$\\x", scanner.getScannedContent());
    }

    @Test
    public void hex_4() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\x{cF}");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("", scanner.getCurrentContent());
        assertEquals("$\\x{cF}", scanner.getScannedContent());
    }

    @Test
    public void hex_5() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\x12");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("", scanner.getCurrentContent());
        assertEquals("$\\x12", scanner.getScannedContent());
    }

    @Test
    public void hex_6() {
        final ErlangCharRule rule = new ErlangCharRule(token);
        scanner = new StringCharacterScanner("$\\x1");
        final IToken tk = rule.evaluate(scanner);
        assertEquals(token, tk);
        assertEquals("", scanner.getCurrentContent());
        assertEquals("$\\x1", scanner.getScannedContent());
    }

}
