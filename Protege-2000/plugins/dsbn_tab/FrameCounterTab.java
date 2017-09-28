
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import edu.stanford.smi.protege.widget.*;
// an example tab
public class FrameCounterTab extends AbstractTabWidget {
    private JButton button;
    private JTextField field;

    // startup code
    public void initialize() {
        // initialize the tab text
        setLabel("Frame Counter");

        // create the button
        button = new JButton("Update Frame Counter");
        button.addActionListener(new ActionListener() {
            // called when the button is pressed
            public void actionPerformed(ActionEvent event) {
                int count = getKnowledgeBase().getFrameCount();
                field.setText(String.valueOf(count));
            }
        });

        // create the output text field
        field = new JTextField(10);
        field.setEnabled(false);
	field.setHorizontalAlignment(JTextField.RIGHT);

        // add the components to the tab widget
	setLayout(new FlowLayout());
	add(button);
        add(field);
    }
    // this method is useful for debugging
    public static void main(String[] args) {
        edu.stanford.smi.protege.Application.main(args);
    }
}


