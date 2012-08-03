package org.erlide.backend;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.erlide.backend.NodeHostClassifier.HostnameType;
import org.erlide.backend.NodeHostClassifier.NodeType;
import org.junit.Test;

public class NodeHostClassifierTests {

    @Test
    public void emptyNameIsStandalone() {
        final NodeHostClassifier state = new NodeHostClassifier("", "any",
                "any.com");
        assertThat(state.mode, is(NodeType.LOCAL_STANDALONE));
        assertThat(state.host, is(HostnameType.NONE));
    }

    @Test
    public void simpleName() {
        final NodeHostClassifier state = new NodeHostClassifier("main", "any",
                "any.com");
        assertThat(state.mode, is(NodeType.LOCAL_DISTRIBUTED));
        assertThat(state.host, is(HostnameType.NONE));
    }

    @Test
    public void simpleHost() {
        final NodeHostClassifier state = new NodeHostClassifier("main@foo",
                "any", "any.com");
        assertThat(state.mode, is(NodeType.REMOTE));
        assertThat(state.host, is(HostnameType.SHORT));
    }

    @Test
    public void fullHost() {
        final NodeHostClassifier state = new NodeHostClassifier("main@foo.bar",
                "any", "any.com");
        assertThat(state.mode, is(NodeType.REMOTE));
        assertThat(state.host, is(HostnameType.LONG));
    }

    @Test
    public void simpleLocalHost() {
        final NodeHostClassifier state = new NodeHostClassifier("main@any",
                "any", "any.com");
        assertThat(state.mode, is(NodeType.LOCAL_DISTRIBUTED));
        assertThat(state.host, is(HostnameType.SHORT));
    }

    @Test
    public void fullLocalHost() {
        final NodeHostClassifier state = new NodeHostClassifier("main@any.com",
                "any", "any.com");
        assertThat(state.mode, is(NodeType.LOCAL_DISTRIBUTED));
        assertThat(state.host, is(HostnameType.LONG));
    }

}
