
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'COMMENT SMTHELSEexpression : COMMENTexpression : SMTHELSE\n    expression : expression expression\n    '
    
_lr_action_items = {'COMMENT':([0,1,2,3,4,],[2,2,-1,-2,2,]),'SMTHELSE':([0,1,2,3,4,],[3,3,-1,-2,3,]),'$end':([1,2,3,4,],[0,-1,-2,-3,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'expression':([0,1,4,],[1,4,4,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> expression","S'",1,None,None,None),
  ('expression -> COMMENT','expression',1,'p_expression_comment','main.py',33),
  ('expression -> SMTHELSE','expression',1,'p_expression_SMTHELSE','main.py',37),
  ('expression -> expression expression','expression',2,'p_expression','main.py',43),
]