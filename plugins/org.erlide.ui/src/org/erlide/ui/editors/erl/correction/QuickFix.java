package org.erlide.ui.editors.erl.correction;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.Pure;

@Accessors
@SuppressWarnings("all")
public class QuickFix implements IMarkerResolution2 {
    private String label;

    private String description;

    private Image image;

    private List<String> tags;

    private QuickFixExecutor executor;

    private List<String> args;

    public QuickFix() {
    }

    public QuickFix(final QuickFix other) {
        label = other.getLabel();
        description = other.getDescription();
        image = other.getImage();
        tags = other.getTags();
        executor = other.getExecutor();
    }

    @Override
    public void run(final IMarker marker) {
        if (marker != null && !marker.exists()) {
            return;
        }
        executor.run(marker, this);
    }

    public boolean appliesAt(final IQuickAssistInvocationContext invocationContext) {
        return executor.appliesAt(invocationContext);
    }

    @Override
    @Pure
    public String getLabel() {
        return label;
    }

    public void setLabel(final String label) {
        this.label = label;
    }

    @Override
    @Pure
    public String getDescription() {
        return description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    @Override
    @Pure
    public Image getImage() {
        return image;
    }

    public void setImage(final Image image) {
        this.image = image;
    }

    @Pure
    public List<String> getTags() {
        return tags;
    }

    public void setTags(final List<String> tags) {
        this.tags = tags;
    }

    @Pure
    public QuickFixExecutor getExecutor() {
        return executor;
    }

    public void setExecutor(final QuickFixExecutor executor) {
        this.executor = executor;
    }

    @Pure
    public List<String> getArgs() {
        return args;
    }

    public void setArgs(final List<String> args) {
        this.args = args;
    }
}
