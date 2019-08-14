/*
 * opencog/atoms/value/QueueValue.h
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_QUEUE_VALUE_H
#define _OPENCOG_QUEUE_VALUE_H

#include <string>
#include <vector>
#include <queue>
#include <mutex>
#include <condition_variable>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

template <typename T>
class ClosableQueue
{
private:
	std::queue<T> _queue;
	std::mutex _mutex;
	std::condition_variable _cond;
	bool _closed = false;

public:

  void push(const T& item)
  {
    std::unique_lock<std::mutex> mlock(_mutex);
    _queue.push(item);
    mlock.unlock();
    _cond.notify_one();
  }

  void push(T&& item)
  {
    std::unique_lock<std::mutex> mlock(_mutex);
    _queue.push(std::move(item));
    mlock.unlock();
    _cond.notify_one();
  }

  bool pop(T& item)
  {
    std::unique_lock<std::mutex> mlock(_mutex);

    while (_queue.empty())
    {
      if(_closed) return false;
      _cond.wait(mlock);
    }

    item = _queue.front();
    _queue.pop();

    return true;
  }

  void close()
  {
	  std::unique_lock<std::mutex> mlock(_mutex);
	  _closed = true;
	  mlock.unlock();
	  _cond.notify_all();
  }
};

typedef ClosableQueue<Handle> HandleClosableQueue;
typedef std::shared_ptr<HandleClosableQueue> HandleClosableQueuePtr;

/**
 * ClosableQueueValue contains a queue that allows to pass values from
 * a producer to consumer.
 */

class QueueValue
	: public Value
{
protected:
	std::string _name;
	HandleClosableQueuePtr _handle_queue;

public:

	static const Handle CONTROL_KEY;

	QueueValue(const std::string& name)
		: Value(QUEUE_VALUE),
		_name(name),
		_handle_queue(new HandleClosableQueue()) { }

	virtual ~QueueValue() {}

	HandleClosableQueuePtr get_queue() const { return _handle_queue; }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if the two atoms are equal.  */
	virtual bool operator==(const Value&) const;
};


typedef std::shared_ptr<const QueueValue> QueueValuePtr;
static inline QueueValuePtr QueueValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<const QueueValue>(a); }

template<typename ... Type>
static inline std::shared_ptr<QueueValue> createQueueValue(Type&&... args) {
	return std::make_shared<QueueValue>(std::forward<Type>(args)...);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_QUEUE_VALUE_H
