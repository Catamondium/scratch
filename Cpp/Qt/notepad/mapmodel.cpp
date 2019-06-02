#include "mapmodel.h"

MapModel::MapModel(QObject *parent)
    : QAbstractTableModel(parent)
{
    _map = NULL;
}

QVariant MapModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if(role != Qt::DisplayRole)
        return QVariant();

    if(orientation == Qt::Horizontal) {
        switch(section) {
        case 0:
            return tr("Char");
        case 1:
            return tr("Count");
        default:
            return "Unknown";
        }
    }

    return QVariant();
}

int MapModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;

    if(_map)
        return _map->count();
    return 0;
}

int MapModel::columnCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;

    return 2;
}

QVariant MapModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    if(!_map)
        return QVariant();

    if(index.row() < 0 ||
       index.row() >= _map->count() ||
            role != Qt::DisplayRole) {
        return QVariant();
    }

    if(index.column() == 0)
        return _map->keys().at(index.row());
    if(index.column() == 1)
        return _map->values().at(index.row());
    return QVariant();
}
