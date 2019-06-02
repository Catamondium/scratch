#ifndef MAPMODEL_H
#define MAPMODEL_H

#include <QAbstractTableModel>
#include <QMap>

class MapModel : public QAbstractTableModel
{
    Q_OBJECT

public:
    enum MapRole {
        KeyRole = Qt::UserRole + 1,
        ValueRole
    };

    explicit MapModel(QObject *parent = nullptr);

    // Header:
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

    // Basic functionality:
    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
    inline void setMap(QMap<QChar, int> *map) {
        beginResetModel();
        _map = map;
        endResetModel();
    }

    inline void notify() {
        beginResetModel();
        endResetModel();
    }

private:
    QMap<QChar, int> *_map;
};

#endif // MAPMODEL_H
