
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'CHAR NEWLINE SPACEexpression : CHARexpression : SPACEexpression : NEWLINE\n    expression : expression expression\n    '
    
_lr_action_items = {'CHAR':([0,1,2,3,4,5,],[2,2,-1,-2,-3,2,]),'SPACE':([0,1,2,3,4,5,],[3,3,-1,-2,-3,3,]),'NEWLINE':([0,1,2,3,4,5,],[4,4,-1,-2,-3,4,]),'$end':([1,2,3,4,5,],[0,-1,-2,-3,-4,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'expression':([0,1,5,],[1,5,5,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> expression","S'",1,None,None,None),
  ('expression -> CHAR','expression',1,'p_expression_char','main.py',47),
  ('expression -> SPACE','expression',1,'p_expression_space','main.py',51),
  ('expression -> NEWLINE','expression',1,'p_expression_newline','main.py',55),
  ('expression -> expression expression','expression',2,'p_expression','main.py',60),
]
