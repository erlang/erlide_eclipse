package org.erlide.core.preferences;

import java.util.Arrays;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.engine.util.PreferencesHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.osgi.service.prefs.BackingStoreException;

public class PreferencesHelperTest {

    private static final IScopeContext[] ALL_SCOPE_CONTEXTS = {
            InstanceScope.INSTANCE, ConfigurationScope.INSTANCE, DefaultScope.INSTANCE };
    private static final String QUALIFIER = "org.erlide.testing";
    private static final String KEY = "key";

    @Before
    public void setUp() throws BackingStoreException {
        InstanceScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).removeNode();
        ConfigurationScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).removeNode();
        DefaultScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).removeNode();
        Platform.getPreferencesService().getRootNode().flush();
    }

    @Test
    public void nextContexts_1() {
        final IScopeContext[] list = PreferencesHelperTest.ALL_SCOPE_CONTEXTS;
        final IScopeContext item = ConfigurationScope.INSTANCE;
        final IScopeContext[] val = PreferencesHelper.getNextContexts(list, item);
        Assert.assertNotNull(val);
        Assert.assertEquals(1, val.length);
        Assert.assertEquals(DefaultScope.INSTANCE, val[0]);
    }

    @Test
    public void nextContexts_2() {
        final IScopeContext[] list = PreferencesHelperTest.ALL_SCOPE_CONTEXTS;
        final IScopeContext item = DefaultScope.INSTANCE;
        final IScopeContext[] val = PreferencesHelper.getNextContexts(list, item);
        Assert.assertNotNull(val);
        Assert.assertEquals(0, val.length);
    }

    @Test
    public void nextContexts_3() {
        final IScopeContext[] list = PreferencesHelperTest.ALL_SCOPE_CONTEXTS;
        final IScopeContext item = InstanceScope.INSTANCE;
        final IScopeContext[] val = PreferencesHelper.getNextContexts(list, item);
        Assert.assertNotNull(val);
        Assert.assertEquals(2, val.length);
        Assert.assertEquals(ConfigurationScope.INSTANCE, val[0]);
        Assert.assertEquals(DefaultScope.INSTANCE, val[1]);
    }

    @Test
    public void string_set() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        helper.putString(PreferencesHelperTest.KEY, "smurf");
        final String res = helper.getString(PreferencesHelperTest.KEY, "default");
        Assert.assertEquals("smurf", res);
    }

    @Test
    public void default_0() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        helper.putString(PreferencesHelperTest.KEY, "balm");
        String res = InstanceScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNotNull(res);
        res = ConfigurationScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNull(res);
        res = DefaultScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNull(res);
    }

    @Test
    public void default_1() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        DefaultScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).put(PreferencesHelperTest.KEY, "balm");
        helper.putString(PreferencesHelperTest.KEY, "balm");
        String res = InstanceScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNull(res);
        res = ConfigurationScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNull(res);
        res = DefaultScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNotNull(res);
    }

    @Test
    public void default_2() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        ConfigurationScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).put(PreferencesHelperTest.KEY, "balm");
        helper.putString(PreferencesHelperTest.KEY, "balm");
        String res = InstanceScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNull(res);
        res = ConfigurationScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNotNull(res);
        res = DefaultScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNull(res);
    }

    @Test
    public void default_3() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        ConfigurationScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).put(PreferencesHelperTest.KEY, "balm");
        helper.putString(PreferencesHelperTest.KEY, "smurf");
        String res = InstanceScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNotNull(res);
        res = ConfigurationScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNotNull(res);
        res = DefaultScope.INSTANCE.getNode(PreferencesHelperTest.QUALIFIER).get(PreferencesHelperTest.KEY, null);
        Assert.assertNull(res);
    }

    @Test
    public void byteArray_set() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        final byte[] val = { 1, 3, 5, 7 };
        helper.putByteArray(PreferencesHelperTest.KEY, val);
        final byte[] res = helper.getByteArray(PreferencesHelperTest.KEY, null);
        Assert.assertEquals(Arrays.toString(val), Arrays.toString(res));
    }

    @Test
    public void double_set() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        final double val = 3.1415926d;
        helper.putDouble(PreferencesHelperTest.KEY, val);
        final double res = helper.getDouble(PreferencesHelperTest.KEY, Double.NaN);
        Assert.assertEquals(val, res, 1.0e-5);
    }

    @Test
    public void float_set() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        final float val = 3.1415926f;
        helper.putFloat(PreferencesHelperTest.KEY, val);
        final float res = helper.getFloat(PreferencesHelperTest.KEY, Float.NaN);
        Assert.assertEquals(val, res, 1.0e-5);
    }

    @Test
    public void long_set() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        final long val = 314159260;
        helper.putLong(PreferencesHelperTest.KEY, val);
        final long res = helper.getLong(PreferencesHelperTest.KEY, Long.MIN_VALUE);
        Assert.assertEquals(val, res);
    }

    @Test
    public void int_set() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        final int val = 314159;
        helper.putInt(PreferencesHelperTest.KEY, val);
        final int res = helper.getInt(PreferencesHelperTest.KEY, Integer.MIN_VALUE);
        Assert.assertEquals(val, res);
    }

    @Test
    public void removeLowestLevel() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        final String val = "value";
        final String defaultValue = "defdef";
        helper.putString(PreferencesHelperTest.KEY, val);
        String res = helper.getString(PreferencesHelperTest.KEY, defaultValue);
        Assert.assertEquals(val, res);

        helper.removeAllAtLowestScope();
        res = helper.getString(PreferencesHelperTest.KEY, defaultValue);
        Assert.assertEquals(defaultValue, res);
    }

    @Test
    public void hasAnyLowestLevel_no1() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        final boolean has = helper.hasAnyAtLowestScope();
        Assert.assertEquals(false, has);
    }

    @Test
    public void hasAnyLowestLevel_yes() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        helper.putString(PreferencesHelperTest.KEY, "gaga");
        final boolean has = helper.hasAnyAtLowestScope();
        Assert.assertEquals(true, has);
    }

    @Test
    public void hasAnyLowestLevel_no2() {
        final PreferencesHelper helper = PreferencesHelper.getHelper(PreferencesHelperTest.QUALIFIER);
        helper.putString(PreferencesHelperTest.KEY, "gaga");
        helper.removeAllAtLowestScope();
        final boolean has = helper.hasAnyAtLowestScope();
        Assert.assertEquals(false, has);
    }
}
