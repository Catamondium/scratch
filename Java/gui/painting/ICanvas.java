package painting;

import java.awt.*;
import java.awt.event.*;
import java.awt.color.*;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.event.*;
import java.util.Vector;
import java.util.Random;

class Oval {
    public int x;
    public int y;
    public int radius;

    public Oval(int x, int y, int radius) {
        this.x = x;
        this.y = y;
        this.radius = radius;
    }

    public String toString() {
        return String.format("Oval(x=%d, y=%d, r=%d)", x, y, radius);
    }

    public void draw(Graphics g) {
        int diameter = radius * 2;
        g.fillOval(x - radius, y - radius, diameter, diameter);
    }
}

public class ICanvas extends JPanel implements MouseInputListener {
    private Vector<Oval> ovals = new Vector<Oval>();
    static final long serialVersionUID = 0xbeef;

    public ICanvas() {
        this.addMouseListener(this);
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Random rand = new Random();

        for (Oval o : ovals) {
            // components: int[20, 255)
            Color c = new Color(rand.nextInt(235) + 20, rand.nextInt(235) + 20, rand.nextInt(235) + 20);
            g.setColor(c);
            o.draw(g);
        }
    }

    public void mouseClicked(MouseEvent e) {
        if (e.getButton() == MouseEvent.BUTTON1) {
            ovals.add(new Oval(e.getX(), e.getY(), 20));
            System.out.println("-----------");
            for (Oval o : ovals) {
                System.out.println(o);
            }
        } else {
            ovals.clear();
            System.out.println("Cleared");
        }
        super.repaint();
    }

    public static void main(String[] argv) {
        ICanvas canvas = new ICanvas();
        JFrame app = new JFrame("paintable");

        app.setSize(300, 300);
        app.add(canvas, BorderLayout.CENTER);
        app.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        app.setVisible(true);
    }

    // stubs
    public void mousePressed(MouseEvent e) {

    }

    public void mouseReleased(MouseEvent e) {

    }

    public void mouseEntered(MouseEvent e) {

    }

    public void mouseExited(MouseEvent e) {

    }

    public void mouseMoved(MouseEvent e) {

    }

    public void mouseDragged(MouseEvent e) {

    }
}
