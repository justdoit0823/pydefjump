# -*- coding: utf-8 -*-


import ast
import tokenize
from epc.server import EPCServer
from collections import deque


file_def_cache = {}


def get_file_def_list(*args):
    filename = args[0]
    with open(filename) as f:
        source = f.read()
    main_node = ast.parse(source)
    node_list = []
    for node in main_node.body:
        def_list = parse_def_node(node)
        if def_list:
            node_list.append(def_list)
    return list(filter(bool, node_list))


def get_file_def_pos(filename, keyword=None):

    if filename not in file_def_cache:
        # no cache
        def_map = refresh_file_def_pos(filename)
    else:
        def_map = file_def_cache[filename]

    if not keyword:
        return tuple(def_map.keys())
    return def_map[keyword]


def refresh_file_def_pos(filename):
    def_list = get_file_def_list(filename)
    def_token = get_file_def_token(filename)
    def_map = iter_def_list(def_list, def_token)
    file_def_cache[filename] = def_map
    return def_map


def iter_def_list(def_list, token_map, rootclass=None):
    def_map = {}
    for d in def_list:
        def_type = d[0]
        if rootclass is None:
            def_map[d[1]] = token_map[d[1]]
        else:
            new_name = rootclass + '.' + d[1]
            def_map[new_name] = token_map[new_name]

        if def_type == 'class':
            # sub def in class
            def_map.update(iter_def_list(d[-1], token_map, d[1]))
    return def_map


def echo(*args):
    return args


def parse_def_node(node):
    if isinstance(node, ast.FunctionDef):
        # function node
        return ('function', node.name, node.lineno, node.col_offset)
    elif isinstance(node, ast.ClassDef):
        # class node
        children_list = []
        for n in node.body:
            def_list = parse_def_node(n)
            if def_list:
                children_list.append(def_list)
        return ('class', node.name, node.lineno, node.col_offset, children_list)
    return ()


def get_file_def_token(filename):
    readline = open(filename).readline
    token_generator = tokenize.generate_tokens(readline)
    care_name_set = ('def', 'class')
    token_set1 = (tokenize.INDENT, tokenize.DEDENT)
    token_set2 = (tokenize.NAME, tokenize.INDENT, tokenize.DEDENT)
    function_def = False
    class_def = False
    function_indent = False
    class_indent = False
    token_map = {}
    class_queue = deque()
    function_queue = deque()
    class_function_queue = deque()
    root_class_name = ''

    while True:
        try:
            token_type, token_str, start, end, __ = next(token_generator)
        except StopIteration:
            break
        if not class_def:
            # no class defined before

            if not function_def:
                # no function defined before
                if token_type != tokenize.NAME or\
                   token_str not in care_name_set:
                    continue

                def_token_str = token_str
                token_type, token_str, start, end, __ = next(token_generator)
                if token_type != tokenize.NAME:
                    # irregular defination
                    continue

                if def_token_str == 'def':
                    # def function block start
                    function_def = True
                    token_map[token_str] = start
                elif def_token_str == 'class':
                    # def class block start
                    class_def = True
                    class_name = token_str
                    token_map[class_name] = start
            else:
                if token_type not in token_set2:
                    continue

                if token_type == tokenize.NAME:
                    if token_str not in care_name_set or function_indent:
                        continue

                    def_token_str = token_str
                    token_type, token_str, start, end, __ = next(token_generator)
                    if token_type != tokenize.NAME:
                        # irregular defination
                        continue

                    if def_token_str == 'def':
                        # def function block start
                        function_def = True
                        token_map[token_str] = start
                    elif def_token_str == 'class':
                        # def class block start
                        class_def = True
                        class_name = token_str
                        token_map[class_name] = start

                elif token_type == tokenize.INDENT:
                    # indent here
                    if not function_indent:
                        function_indent = True
                    else:
                        function_queue.append(True)
                elif token_type == tokenize.DEDENT and function_indent:
                    # dedent here
                    try:
                        function_queue.pop()
                    except IndexError:
                        function_indent = False
                        function_def = False
        else:
            # class defined before
            if not function_def:
                # no function defined in class
                if token_type not in token_set2:
                    continue

                if token_type == tokenize.INDENT:
                    # indent here
                    class_indent = True
                    class_queue.append((root_class_name, class_name, True))
                elif token_type == tokenize.NAME:
                    if token_str not in care_name_set:
                        continue

                    def_token_str = token_str
                    token_type, token_str, start, end, __ = next(
                        token_generator)
                    if token_type != tokenize.NAME:
                        # irregular defination
                        continue

                    root_class_prefix = '.'.join(filter(bool, (q[1] for q in class_queue)))

                    if def_token_str == 'def':
                        # def function in class
                        function_def = True
                        if not class_indent:
                            token_map[token_str] = start
                            continue

                        new_token_key = root_class_prefix + '.' + token_str
                        token_map[new_token_key] = start
                    elif def_token_str == 'class':
                        # def class in class
                        class_def = True
                        if not class_indent:
                            class_name = token_str
                            token_map[class_name] = start
                            continue

                        new_token_key = root_class_prefix + '.' + token_str
                        token_map[new_token_key] = start
                        root_class_name = class_name
                        class_name = token_str
                elif token_type == tokenize.DEDENT and class_indent:
                    # dedent here
                    try:
                        root_class_name, __, class_indent = class_queue.pop()
                        class_name = root_class_name
                        class_def = bool(class_name)
                    except IndexError:
                        root_class_name = ''
                        class_def = False
                    class_indent = class_def
            else:
                # function defined in class
                if token_type not in token_set1:
                    continue

                if token_type == tokenize.INDENT:
                    # indent here
                    if not function_indent:
                        function_indent = True
                    else:
                        class_function_queue.append(True)
                elif token_type == tokenize.DEDENT and function_indent:
                    # dedent here
                    try:
                        class_function_queue.pop()
                    except IndexError:
                        function_indent = False
                        function_def = False

    return token_map


def main():
    server  = EPCServer(('localhost', 9898))
    server.register_function(echo)
    server.register_function(get_file_def_pos)
    server.register_function(refresh_file_def_pos)
    server.print_port()
    server.serve_forever()


if __name__ == '__main__':
    main()
