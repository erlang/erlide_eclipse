package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.List;

import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.Test;

public class RecordDefTests extends ErlModelTestBase {

    @Test
    public void detectRecordName() throws Exception {
        final IErlModule module2 = ErlideTestUtils.createModule(project, "yy.erl",
                "-module(yy).\n" + "-record(X, {x}).\n" + "-record(X , {x}).\n"
                        + "f()->?X.\n");
        module2.open(null);
        final List<IErlElement> childrenOfKind = module2
                .getChildrenOfKind(ErlElementKind.RECORD_DEF);

        IErlRecordDef def = (IErlRecordDef) childrenOfKind.get(0);
        assertThat(def.getDefinedName(), is("X"));

        def = (IErlRecordDef) childrenOfKind.get(1);
        assertThat(def.getDefinedName(), is("X"));
    }

}
