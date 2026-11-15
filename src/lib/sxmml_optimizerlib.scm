;; IR の型と固定比較は optimizer の責務。圧縮コアには C 関数だけを渡す。
(in-module sxmml.native)
(inline-stub
 (.include "sxmml_native.h" <string.h>)

 (define-cfn ir_class (ctx::SxmmlIrContext* obj::ScmObj) ::ScmClass* :static
   (let* ([klass::ScmClass* (Scm_ClassOf obj)])
     (if (SCM_FALSEP (-> klass redefined))
       (return klass)
       (return (SCM_CLASS (Scm_ApplyRec1 (-> ctx class_of) obj))))))

 (define-cfn ir_accessors (ctx::SxmmlIrContext* klass::ScmClass*) ::ScmObj :static
   (let* ([key::ScmObj (SCM_OBJ klass)]
          [table::ScmHashTable* (SCM_HASH_TABLE (-> ctx accessors))]
          [found::ScmObj (Scm_HashTableRef table key SCM_FALSE)])
     (when (SCM_FALSEP found)
       (set! found (Scm_ApplyRec1 (-> ctx class_accessors) key))
       (Scm_HashTableSet table key found 0))
     (return found)))

 (define-cfn ir_slot (ctx::SxmmlIrContext* obj::ScmObj sa::ScmSlotAccessor* boundp::int) ::ScmObj :static
   (if (and (== (-> sa getter) NULL) (>= (-> sa slotNumber) 0)
            (== (-> sa klass) (ir_class ctx obj)))
     (let* ([value::ScmObj (Scm_InstanceSlotRef obj (-> sa slotNumber) SCM_UNDEFINED)])
       (if boundp (return (SCM_MAKE_BOOL (not (SCM_UNDEFINEDP value)))) (return value)))
     (return (Scm_ApplyRec2 (?: boundp (-> ctx slot_bound) (-> ctx slot_ref)) obj (-> sa name)))))

 (define-cfn ir_value (ctx::SxmmlIrContext* obj::ScmObj) ::ScmObj :static
   (let* ([slots::ScmObj (ir_accessors ctx (ir_class ctx obj))])
     (dotimes [i (SCM_VECTOR_SIZE slots)]
       (let* ([sa::ScmSlotAccessor* (SCM_SLOT_ACCESSOR (SCM_VECTOR_ELEMENT slots i))])
         (when (SCM_EQ (-> sa name) (SCM_INTERN "value"))
           (return (ir_slot ctx obj sa FALSE)))))
     (return (Scm_ApplyRec2 (-> ctx slot_ref) obj (SCM_INTERN "value")))))

 (define-cfn ir_items_same (ctx::SxmmlIrContext* a::ScmObj b::ScmObj) ::int :static
   (while (and (SCM_PAIRP a) (SCM_PAIRP b))
     (unless (SxmmlNativeIrSame ctx (SCM_CAR a) (SCM_CAR b)) (return FALSE))
     (set! a (SCM_CDR a)) (set! b (SCM_CDR b)))
   (return (and (SCM_NULLP a) (SCM_NULLP b))))

 (define-cfn SxmmlNativeIrSame (ctx::SxmmlIrContext* a::ScmObj b::ScmObj) ::int
   (let* ([ba::int (Scm_TypeP a (SCM_CLASS (-> ctx block_class)))]
          [bb::int (Scm_TypeP b (SCM_CLASS (-> ctx block_class)))])
     (when (or ba bb)
       (unless (and ba bb) (return FALSE))
       (let* ([enda::ScmObj (Scm_InstanceSlotRef a (-> ctx block_end) SCM_UNDEFINED)]
              [endb::ScmObj (Scm_InstanceSlotRef b (-> ctx block_end) SCM_UNDEFINED)]
              [breaka::ScmObj (Scm_InstanceSlotRef a (-> ctx block_break) SCM_UNDEFINED)]
              [breakb::ScmObj (Scm_InstanceSlotRef b (-> ctx block_break) SCM_UNDEFINED)]
              [backa::ScmObj (Scm_InstanceSlotRef a (-> ctx block_back) SCM_UNDEFINED)]
              [backb::ScmObj (Scm_InstanceSlotRef b (-> ctx block_back) SCM_UNDEFINED)])
         (return
          (and (Scm_EqualP (ir_value ctx enda) (ir_value ctx endb))
               (== (SCM_FALSEP breaka) (SCM_FALSEP breakb))
               (ir_items_same ctx (Scm_InstanceSlotRef a (-> ctx block_front) SCM_UNDEFINED)
                                 (Scm_InstanceSlotRef b (-> ctx block_front) SCM_UNDEFINED))
               (ir_items_same ctx (?: (SCM_FALSEP backa) SCM_NIL backa)
                                 (?: (SCM_FALSEP backb) SCM_NIL backb)))))))
   (when (or (SCM_PAIRP a) (SCM_PAIRP b))
     (return (and (SCM_PAIRP a) (SCM_PAIRP b) (ir_items_same ctx a b))))
   (let* ([ca::ScmClass* (ir_class ctx a)] [cb::ScmClass* (ir_class ctx b)])
     (unless (Scm_EqualP (SCM_OBJ ca) (SCM_OBJ cb)) (return FALSE))
     (let* ([slots::ScmObj (ir_accessors ctx ca)])
       (dotimes [i (SCM_VECTOR_SIZE slots)]
         (let* ([sa::ScmSlotAccessor* (SCM_SLOT_ACCESSOR (SCM_VECTOR_ELEMENT slots i))]
                [ba::int (SCM_TRUEP (ir_slot ctx a sa TRUE))]
                [bb::int (SCM_TRUEP (ir_slot ctx b sa TRUE))])
           (when (!= ba bb) (return FALSE))
           (when (and ba (not (Scm_EqualP (ir_slot ctx a sa FALSE) (ir_slot ctx b sa FALSE))))
             (return FALSE))))))
   (return TRUE))

 (define-cfn block_slot (slots::ScmObj (name :: (const char*))) ::int :static
   (dotimes [i (SCM_VECTOR_SIZE slots)]
     (let* ([sa::ScmSlotAccessor* (SCM_SLOT_ACCESSOR (SCM_VECTOR_ELEMENT slots i))])
       (when (SCM_EQ (-> sa name) (SCM_INTERN name)) (return (-> sa slotNumber)))))
   (Scm_Error "repeat block field is not defined: %s" name)
   (return -1))

 (define-cfn make_ir_context () ::SxmmlIrContext* :static
   (let* ([ctx::SxmmlIrContext* (SCM_NEW SxmmlIrContext)])
     (memset ctx 0 (sizeof SxmmlIrContext))
     (set! (-> ctx block_class) (SxmmlNativeBinding "sxmml.optimizer" "<repeat-block>"))
     (set! (-> ctx class_accessors) (SxmmlNativeBinding "sxmml.optimizer" "%repeat-class-accessors"))
     (set! (-> ctx class_of) (SxmmlNativeBinding "gauche" "class-of"))
     (set! (-> ctx slot_bound) (SxmmlNativeBinding "gauche" "slot-bound?"))
     (set! (-> ctx slot_ref) (SxmmlNativeBinding "gauche" "slot-ref"))
     (set! (-> ctx accessors) (Scm_MakeHashTableSimple SCM_HASH_EQ 0))
     (let* ([slots::ScmObj (ir_accessors ctx (SCM_CLASS (-> ctx block_class)))])
       (set! (-> ctx block_front) (block_slot slots "front"))
       (set! (-> ctx block_back) (block_slot slots "back"))
       (set! (-> ctx block_break) (block_slot slots "break"))
       (set! (-> ctx block_end) (block_slot slots "end")))
     (return ctx)))

 (define-cfn ir_same_indices (data::void* left::size_t right::size_t) ::int :static
   (let* ([ctx::SxmmlIrContext* (cast SxmmlIrContext* data)])
     (return (SxmmlNativeIrSame ctx (SCM_VECTOR_ELEMENT (-> ctx items) left)
                                 (SCM_VECTOR_ELEMENT (-> ctx items) right)))))

 (define-cfn ir_same_procedure (args::ScmObj* argc::int data::void*) ::ScmObj :static
   (cast void argc)
   (return (SCM_MAKE_BOOL (SxmmlNativeIrSame (cast SxmmlIrContext* data)
                                           (aref args 0) (aref args 1)))))

 (define-cproc sxmml-native-ir-compress (items::<vector> weights::<vector> settings)
   (let* ([ctx::SxmmlIrContext* (make_ir_context)])
     (set! (-> ctx items) (SCM_OBJ items))
     (return (SxmmlNativeCompress (SCM_OBJ items) (SCM_OBJ weights) settings ir_same_indices ctx))))

 (define-cproc make-sxmml-native-ir-comparison ()
   (return (Scm_MakeSubr ir_same_procedure (make_ir_context) 2 0 SCM_FALSE))))
