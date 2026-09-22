/*
 * opencog/atoms/plan9/NineNamespace.cc
 *
 * Copyright (c) 2024 OpenCog Foundation
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

#include <opencog/atoms/plan9/NineNamespace.h>

#include <algorithm>
#include <stdexcept>
#include <unordered_set>

namespace opencog
{
namespace plan9
{

std::vector<std::string> NineNamespace::split(const std::string& path)
{
	std::vector<std::string> out;
	std::string cur;
	for (char c : path)
	{
		if (c == '/')
		{
			if (not cur.empty()) { out.push_back(cur); cur.clear(); }
		}
		else cur.push_back(c);
	}
	if (not cur.empty()) out.push_back(cur);
	return out;
}

std::string NineNamespace::canon(const std::string& path)
{
	std::vector<std::string> comps = split(path);
	if (comps.empty()) return "/";
	std::string out;
	for (const auto& c : comps) { out += "/"; out += c; }
	return out;
}

NineNamespace::NineNamespace(NineNodePtr root)
{
	_mounts["/"] = { std::move(root) };
}

std::vector<NineNodePtr> NineNamespace::walk_union(const std::string& path) const
{
	std::vector<std::string> comps = split(path);

	auto root_it = _mounts.find("/");
	std::vector<NineNodePtr> stack =
		(root_it != _mounts.end()) ? root_it->second : std::vector<NineNodePtr>();
	if (stack.empty())
		return {};

	std::string so_far;
	for (const auto& c : comps)
	{
		so_far += "/";
		so_far += c;

		std::vector<NineNodePtr> next;
		for (const auto& n : stack)
		{
			if (not n or not n->is_dir()) continue;
			NineNodePtr child = n->lookup(c);
			if (child) next.push_back(child);
		}
		if (next.empty())
			return {};
		stack = std::move(next);

		auto ov = _mounts.find(so_far);
		if (ov != _mounts.end() and not ov->second.empty())
			stack = ov->second;
	}
	return stack;
}

NineNodePtr NineNamespace::walk(const std::string& path) const
{
	std::vector<NineNodePtr> stack = walk_union(path);
	return stack.empty() ? nullptr : stack.front();
}

std::vector<NineNodePtr> NineNamespace::list(const std::string& path) const
{
	std::vector<NineNodePtr> stack = walk_union(path);
	std::vector<NineNodePtr> out;
	std::unordered_set<std::string> seen;

	for (const auto& n : stack)
	{
		if (not n or not n->is_dir()) continue;
		for (const auto& child : n->children())
		{
			if (not child) continue;
			if (seen.insert(child->name()).second)
				out.push_back(child);
		}
	}
	return out;
}

void NineNamespace::mount(const std::string& at_path, NineNodePtr device,
                           NineMountOrder order)
{
	std::string canonical = canon(at_path);

	std::vector<NineNodePtr> stack;
	auto it = _mounts.find(canonical);
	if (it != _mounts.end())
		stack = it->second;
	else
		stack = walk_union(canonical);

	switch (order)
	{
		case MREPL:
			stack = { std::move(device) };
			break;
		case MBEFORE:
			stack.insert(stack.begin(), std::move(device));
			break;
		case MAFTER:
			stack.push_back(std::move(device));
			break;
	}
	_mounts[canonical] = std::move(stack);
}

void NineNamespace::bind(const std::string& new_path,
                          const std::string& old_path, NineMountOrder order)
{
	std::vector<NineNodePtr> src = walk_union(canon(old_path));
	if (src.empty())
		throw std::runtime_error("bind: source path not found: " + old_path);

	std::string canonical = canon(new_path);
	std::vector<NineNodePtr> stack;
	auto it = _mounts.find(canonical);
	if (it != _mounts.end())
		stack = it->second;
	else
		stack = walk_union(canonical); // fine if empty: new_path may be fresh

	switch (order)
	{
		case MREPL:
			stack = src;
			break;
		case MBEFORE:
			stack.insert(stack.begin(), src.begin(), src.end());
			break;
		case MAFTER:
			stack.insert(stack.end(), src.begin(), src.end());
			break;
	}
	_mounts[canonical] = std::move(stack);
}

void NineNamespace::unmount(const std::string& at_path)
{
	std::string canonical = canon(at_path);
	if (canonical == "/")
		throw std::runtime_error("unmount: cannot unmount the root");
	_mounts.erase(canonical);
}

} // namespace plan9
} // namespace opencog
