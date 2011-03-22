package org.erlide.core.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.erlide.core.common.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

/**
 * The class <code>StringUtilsTest</code> contains tests for the class
 * <code>{@link StringUtils}</code>.
 * 
 * @generatedBy CodePro at 2010-09-17 14:04
 * @author qvladum
 * @version $Revision: 1.0 $
 */
public class StringUtilsTest {
    /**
     * Run the char[] addChar(char[],char) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testAppendChar_1() throws Exception {
        final char[] c = new char[] {};
        final char toAdd = 'a';

        final char[] result = StringUtils.appendChar(c, toAdd);

        // add additional test code here
        assertNotNull(result);
        assertEquals(1, result.length);
        assertEquals('a', result[0]);
    }

    @Test
    public void testAppendChar_2() throws Exception {
        final char[] c = new char[] { 'a', 'b' };
        final char toAdd = 'c';

        final char[] result = StringUtils.appendChar(c, toAdd);

        // add additional test code here
        assertNotNull(result);
        assertEquals(3, result.length);
        assertEquals("abc", new String(result));
    }

    /**
     * Run the String join(String[]) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoinWithSpaces_1() throws Exception {
        final String[] cmds = new String[] {};

        final String result = StringUtils.joinWithSpaces(cmds);

        // add additional test code here
        assertEquals("", result);
    }

    /**
     * Run the String join(String[]) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoinWithSpaces_2() throws Exception {
        final String[] cmds = new String[] { "a", "b" };

        final String result = StringUtils.joinWithSpaces(cmds);

        // add additional test code here
        assertEquals("a b", result);
    }

    /**
     * Run the String join(String,List<String>) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoin_3() throws Exception {
        final String delimiter = ",";
        final List<String> splitted = Lists.newArrayList();

        final String result = StringUtils.join(delimiter, splitted);

        // add additional test code here
        assertEquals("", result);
    }

    /**
     * Run the String join(String,List<String>) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoin_4() throws Exception {
        final String delimiter = ",";
        final List<String> splitted = Lists.newArrayList();
        splitted.add("a");

        final String result = StringUtils.join(delimiter, splitted);

        // add additional test code here
        assertEquals("a", result);
    }

    /**
     * Run the String join(String,List<String>) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoin_5() throws Exception {
        final String delimiter = ",";
        final List<String> splitted = Lists.newArrayList();
        splitted.add("a");
        splitted.add("b");

        final String result = StringUtils.join(delimiter, splitted);

        // add additional test code here
        assertEquals("a,b", result);
    }

    /**
     * Run the String join(String,String[]) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoin_6() throws Exception {
        final String delimiter = ",";
        final String[] splitted = new String[] {};

        final String result = StringUtils.join(delimiter, splitted);

        // add additional test code here
        assertEquals("", result);
    }

    /**
     * Run the String join(String,String[]) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoin_7() throws Exception {
        final String delimiter = ",";
        final String[] splitted = new String[] { "a" };

        final String result = StringUtils.join(delimiter, splitted);

        // add additional test code here
        assertEquals("a", result);
    }

    /**
     * Run the String join(String,String[]) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testJoin_8() throws Exception {
        final String delimiter = ",";
        final String[] splitted = new String[] { "a", "b" };

        final String result = StringUtils.join(delimiter, splitted);

        // add additional test code here
        assertEquals("a,b", result);
    }

    /**
     * Run the List<String> splitInLines(String) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testSplitInLines_1() throws Exception {
        final String string = "aaa";

        final List<String> result = StringUtils.splitLines(string);

        // add additional test code here
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.contains("aaa"));
    }

    /**
     * Run the List<String> splitInLines(String) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testSplitInLines() throws Exception {
        final String string = "a\nbc";

        final List<String> result = StringUtils.splitLines(string);

        // add additional test code here
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals(result.get(0), "a\n");
        assertEquals(result.get(1), "bc");
    }

    /**
     * Run the List<String> splitInLines(String) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testSplitInLines_3() throws Exception {
        final String string = "aa\n";

        final List<String> result = StringUtils.splitLines(string);

        // add additional test code here
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(result.get(0), "aa\n");
    }

    /**
     * Run the List<String> splitInLines(String) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testSplitInLines_4() throws Exception {
        final String string = "\naa";

        final List<String> result = StringUtils.splitLines(string);

        // add additional test code here
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals(result.get(0), "\n");
        assertEquals(result.get(1), "aa");
    }

    /**
     * Run the List<String> splitInLines(String) method test.
     * 
     * @throws Exception
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testSplitInLines_5() throws Exception {
        final String string = "";

        final List<String> result = StringUtils.splitLines(string);

        // add additional test code here
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    /**
     * Perform pre-test initialization.
     * 
     * @throws Exception
     *             if the initialization fails for some reason
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Before
    public void setUp() throws Exception {
        // add additional set up code here
    }

    /**
     * Perform post-test clean-up.
     * 
     * @throws Exception
     *             if the clean-up fails for some reason
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @After
    public void tearDown() throws Exception {
        // Add additional tear down code here
    }

    /**
     * Launch the test.
     * 
     * @param args
     *            the command line arguments
     * 
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    public static void main(final String[] args) {
        new org.junit.runner.JUnitCore().run(StringUtilsTest.class);
    }
}
