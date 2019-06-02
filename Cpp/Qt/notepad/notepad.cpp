#include "notepad.h"
#include "ui_notepad.h"
#include <QFileDialog>
#include <QFontDialog>

#include <QMessageBox>
#include <QTextStream>

#include <QShortcut>

#include <iostream>
#include <QDebug>

Notepad::Notepad(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::Notepad)
{
    ui->setupUi(this);
    model.setMap(&counter);
    ui->table->setModel(&model);
    ui->table->hide();
}

Notepad::~Notepad()
{
    delete ui;
}

void Notepad::on_actionOpen_triggered()
{
    // Why're you truncating everything you open?
    QString filename = QFileDialog::getOpenFileName(this, "Select file");
    QFile file(filename);
    currentFile = filename;
    if(!file.open(QIODevice::ReadOnly | QFile::Text)) {
        QMessageBox::warning(this, "Warning", "Cannot open file: " + file.errorString());
        return;
    }
    setWindowTitle(filename);
    QTextStream in(&file);
    QString content = in.readAll();
    ui->maintext->document()->setPlainText(content);
    file.close();

    enumerate();
}

void Notepad::on_actionSave_triggered()
{
    QString fileName;
     // If we don't have a filename from before, get one.
     if (currentFile.isEmpty()) {
         fileName = QFileDialog::getSaveFileName(this, "Save");
         currentFile = fileName;
     } else {
         fileName = currentFile;
     }
     QFile file(fileName);
     if (!file.open(QIODevice::WriteOnly | QFile::Text)) {
         QMessageBox::warning(this, "Warning", "Cannot save file: " + file.errorString());
         return;
     }
     setWindowTitle(fileName);
     QTextStream out(&file);
     QString text = ui->maintext->document()->toPlainText();
     out << text;
     file.close();
}

void Notepad::on_actionSave_As_triggered()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Save as");
    QFile file(fileName);

    if (!file.open(QFile::WriteOnly | QFile::Text)) {
        QMessageBox::warning(this, "Warning", "Cannot save file: " + file.errorString());
        return;
    }
    currentFile = fileName;
    setWindowTitle(fileName);
    QTextStream out(&file);
    QString text = ui->maintext->document()->toPlainText();
    out << text;
    file.close();
}

void Notepad::on_actionChangefont_triggered()
{
    bool fontSelected;
   QFont font = QFontDialog::getFont(&fontSelected, this);
   if(fontSelected) {
       ui->maintext->setFont(font);
   }
}

void Notepad::on_actionNew_triggered()
{
    currentFile.clear();
    ui->maintext->document()->clear();
    setWindowTitle(QApplication::applicationName());
}

void Notepad::on_maintext_textChanged()
{
    enumerate();
}

void Notepad::on_actionRefresh_table_triggered()
{
    enumerate();
}

void Notepad::enumerate() {
    counter.clear();
    QString content = ui->maintext->document()->toPlainText();

    for(int i  = 0; i < content.count(); ++i) {
        if(content[i].isPrint())
            counter[content[i]] += 1;
    }

    model.notify();
    if(counter.count() == 0)
        ui->table->hide();
    else
        ui->table->show();
}
