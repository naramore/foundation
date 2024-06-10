# resource storage
# apiserver
#     supervisor -> gateway
#                -> supervisor -> resource_server
#     objects/resources
#         fields
#             typemeta -> kind, version
#             objectmeta -> name, generatedname, namespace, uid, resourceversion, generation,
#                           creationtimestamp, deletiontimestamp, deletiongraceperiodseconds,
#                           labels, annotations, finalizers
#                           ownerreferences -> apiversion, kind, name, uid, controller, blockownerdeletion
#                           managedfields -> manager, operation, apiversion, time, fieldstype, fieldsv1[raw], subresource
#             spec
#             status
#         functions
#             create(ctx, t, opts) :: {:ok, t} | {:error, reason}
#             update(ctx, t, opts) :: {:ok, t} | {:error, reason}
#             delete(ctx, name, opts) :: :ok | {:error, reason}
#             get(ctx, name, opts) :: {:ok, t} | {:error, reason}
#             list(ctx, opts) :: {:ok, [t]} | {:error, reason}
#             watch(ctx, opts) :: {:ok, watch} | {:error, reason}
#             patch(ctx, name, type, data, opts, subresources) :: {:ok, t} | {:error, reason}
#             apply(ctx, config, opts) :: {:ok, t} | {:error, reason}
#     subresources
#     resource references/links
#     metadata
#     resource definitions
# controller & informer managers / supervisors
# informer genserver/genstatem
# informer behaviour
#     shared_index_informer -> reflector, delta fifo queue, cache controller, indexer, local store, ...
#     multi_resource_informer
# controller run_queue_server
# controller genserver/genstatem
# controller behaviour
#     @type event :: {:add, obj} | {:update, obj, obj} | {:delete, obj}
#     @callback init() :: state
#     @callback handle_event(event, ) :: term
#     @callback reconcile() :: term
#     controller interception (i.e. admission & authn/z)

defmodule Highstorm do
  @moduledoc false
  use Boundary, deps: [], exports: []
end

defmodule Highstorm.Domain do
  @moduledoc false

  # Highstorm.Domain.ResourceServer
  # Highstorm.Domain.ResourceDefinition
  # Highstorm.Domain.Resource
  # Highstorm.Domain.Resource.Spec
  # Highstorm.Domain.Resource.Status
  # Highstorm.Domain.SubResource
  # Highstorm.Domain.Link
  # Highstorm.Domain.Metadata
  # Highstorm.Domain.Event
  # Highstorm.Domain.Informer
  # Highstorm.Domain.Operator
end

defmodule Highstorm.Controllers do
  @moduledoc false

  # Highstorm.Controllers.Application
  # Highstorm.Controllers.Supervisor
end

defmodule Highstorm.PrimaryAdapters do
  @moduledoc false

  # Highstorm.PrimaryAdapters.ResourceServer
  # Highstorm.PrimaryAdapters.ResourceServerSupervisor
  # Highstorm.PrimaryAdapters.ResourceGateway
  # Highstorm.PrimaryAdapters.InformerSupervisor
  # Highstorm.PrimaryAdapters.ControllerSupervisor
end

defmodule Highstorm.SecondaryAdapters do
  @moduledoc false
end

defmodule Highstorm.Glue do
  @moduledoc false
end
