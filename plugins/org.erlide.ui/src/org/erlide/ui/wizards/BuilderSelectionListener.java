package org.erlide.ui.wizards;

import java.util.Collection;

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.core.builder.executor.ToolExecutor;
import org.erlide.engine.NewProjectData;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ProjectConfigType;

import com.google.common.base.Objects;

@SuppressWarnings("all")
public class BuilderSelectionListener implements SelectionListener {
    private final NewProjectData info;

    private final ErlangProjectBuilderPage page;

    public BuilderSelectionListener(final NewProjectData info,
            final ErlangProjectBuilderPage page) {
        this.info = info;
        this.page = page;
    }

    @Override
    public void widgetDefaultSelected(final SelectionEvent e) {
    }

    @Override
    public void widgetSelected(final SelectionEvent e) {
        final Object _data = e.widget.getData();
        info.setBuilder((BuilderTool) _data);
        final Collection<ProjectConfigType> cfgs = info.getBuilder().getMatchingConfigs();
        final boolean _contains = cfgs.contains(info.getConfigType());
        final boolean _not = !_contains;
        if (_not) {
            page.selectConfig(IterableExtensions.<ProjectConfigType> head(cfgs));
        }
        page.setMessage(null);
        final boolean toolExists = info.getBuilder().getOsCommand() == null
                || ToolExecutor.getToolLocation(info.getBuilder().getOsCommand()) != null;
        if (!toolExists) {
            final StringConcatenation _builder = new StringConcatenation();
            _builder.append("The tool \'");
            final String _osCommand = info.getBuilder().getOsCommand();
            _builder.append(_osCommand);
            _builder.append("\' can\'t be found on your system\'s $PATH");
            page.setMessage(_builder.toString(), IMessageProvider.WARNING);
        }
        page.configComposite.setVisible(Objects.equal(info.getBuilder(), BuilderTool.MAKE)
                || Objects.equal(info.getBuilder(), BuilderTool.INTERNAL));
        final BuilderTool _builder_1 = info.getBuilder();
        final boolean _equals = Objects.equal(_builder_1, BuilderTool.MAKE);
        page.makeConfigComposite.setVisible(_equals);
    }
}
