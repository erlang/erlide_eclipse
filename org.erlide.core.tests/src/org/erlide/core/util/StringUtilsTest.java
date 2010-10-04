package org.erlide.core.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

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
        char[] c = new char[] {};
        char toAdd = 'a';

        char[] result = StringUtils.appendChar(c, toAdd);

        // add additional test code here
        assertNotNull(result);
        assertEquals(1, result.length);
        assertEquals('a', result[0]);
    }

    @Test
    public void testAppendChar_2() throws Exception {
        char[] c = new char[] { 'a', 'b' };
        char toAdd = 'c';

        char[] result = StringUtils.appendChar(c, toAdd);

        // add additional test code here
        assertNotNull(result);
        assertEquals(3, result.length);
        assertEquals("abc", new String(result));
    }

    /**
     * Run the int countPercS(String) method test.
     *
     * @throws Exception
     *
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testCountPercS_1() throws Exception {
        String str = "";

        int result = StringUtils.countPercS(str);

        // add additional test code here
        assertEquals(0, result);
    }

    /**
     * Run the int countPercS(String) method test.
     *
     * @throws Exception
     *
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testCountPercS_2() throws Exception {
        String str = "aaa";

        int result = StringUtils.countPercS(str);

        // add additional test code here
        assertEquals(0, result);
    }

    /**
     * Run the int countPercS(String) method test.
     *
     * @throws Exception
     *
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testCountPercS_3() throws Exception {
        String str = "a%sa";

        int result = StringUtils.countPercS(str);

        // add additional test code here
        assertEquals(1, result);
    }

    /**
     * Run the int countPercS(String) method test.
     *
     * @throws Exception
     *
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testCountPercS_4() throws Exception {
        String str = "a%ba";

        int result = StringUtils.countPercS(str);

        // add additional test code here
        assertEquals(0, result);
    }

    /**
     * Run the int countPercS(String) method test.
     *
     * @throws Exception
     *
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    @Test
    public void testCountPercS_5() throws Exception {
        String str = "%s%s";

        int result = StringUtils.countPercS(str);

        // add additional test code here
        assertEquals(2, result);
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
        String[] cmds = new String[] {};

        String result = StringUtils.joinWithSpaces(cmds);

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
        String[] cmds = new String[] {"a","b"};

        String result = StringUtils.joinWithSpaces(cmds);

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
        String delimiter = ",";
        List<String> splitted = Lists.newArrayList();

        String result = StringUtils.join(delimiter, splitted);

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
        String delimiter = ",";
        List<String> splitted = Lists.newArrayList();
        splitted.add("a");

        String result = StringUtils.join(delimiter, splitted);

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
        String delimiter = ",";
        List<String> splitted = Lists.newArrayList();
        splitted.add("a");
        splitted.add("b");

        String result = StringUtils.join(delimiter, splitted);

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
        String delimiter = ",";
        String[] splitted = new String[] {};

        String result = StringUtils.join(delimiter, splitted);

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
        String delimiter = ",";
        String[] splitted = new String[] {"a"};

        String result = StringUtils.join(delimiter, splitted);

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
        String delimiter = ",";
        String[] splitted = new String[] {"a","b"};

        String result = StringUtils.join(delimiter, splitted);

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
        String string = "aaa";

        List<String> result = StringUtils.splitLines(string);

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
        String string = "a\nbc";

        List<String> result = StringUtils.splitLines(string);

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
        String string = "aa\n";

        List<String> result = StringUtils.splitLines(string);

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
        String string = "\naa";

        List<String> result = StringUtils.splitLines(string);

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
        String string = "";

        List<String> result = StringUtils.splitLines(string);

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
    public static void main(String[] args) {
        new org.junit.runner.JUnitCore().run(StringUtilsTest.class);
    }
}
