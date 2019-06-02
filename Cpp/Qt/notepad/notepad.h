#ifndef NOTEPAD_H
#define NOTEPAD_H

#include "mapmodel.h"
#include <QMainWindow>

namespace Ui {
class Notepad;
}

class Notepad : public QMainWindow
{
    Q_OBJECT

public:
    explicit Notepad(QWidget *parent = 0);
    ~Notepad();

private slots:
    void on_actionOpen_triggered();

    void on_actionSave_triggered();

    void on_actionSave_As_triggered();

    void on_actionChangefont_triggered();

    void on_actionNew_triggered();

    void on_maintext_textChanged();

    void on_actionRefresh_table_triggered();

private:
    void enumerate();
    Ui::Notepad *ui;
    QString currentFile;
    QMap<QChar, int> counter;
    MapModel model;
};

#endif // NOTEPAD_H
